{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module SyncState
  (
    SyncAction'
  , SyncConflict'
  , SyncPlan'
  , SyncFile'
  , SyncError(..)
  , SyncM
  , runSyncM
  , SyncEnv(..)
  , SyncState(..)
  , liftGitHub

  , genSyncPlans
  , performSyncActions
  ) where

import           Control.Concurrent.Chan
import           Control.Exception
import qualified Control.Foldl as Fold
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Crypto.Hash as H
import           Data.Bifunctor (first)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Serialize as Ser
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Filesystem.Path.CurrentOS as P
import           GHC.Generics
import qualified Network.GitHub as G
import qualified Network.GitHub.Gist.Sync as S
import qualified Network.GitHub.Types.Gist.Edit as GE
import qualified Network.GitHub.Types.Gist.Create as GC
import qualified Network.HTTP.Client as HTTP
import           Servant.Client (ServantError, BaseUrl, runClientM, ClientEnv(..))
import qualified Turtle as Ttl

-- specialized version of SyncAction and SyncFile
type SyncAction' = S.SyncAction H.MD5
type SyncConflict'   = S.SyncConflict H.MD5
type SyncPlan'   = S.SyncPlan H.MD5
type SyncFile'   = S.SyncFile H.MD5

data SyncError = SyncLogicError T.Text
               | ServantError ServantError
               | OtherException SomeException

deriving instance Show SyncError

newtype SyncM a = SyncM
  { unSyncM :: ReaderT SyncEnv (StateT SyncState (ExceptT SyncError G.GitHub)) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader SyncEnv,  MonadError SyncError )

runSyncM
  :: SyncM a
  -> SyncEnv
  -> SyncState
  -> IO (Either SyncError (a, SyncState))
runSyncM m env@SyncEnv{githubToken=token, manager=mgr, githubHost=host} state = do
  a <- (first ServantError <$> run m) `catch` (return . Left . OtherException)
  return $ join a
  where run = flip runClientM (ClientEnv mgr host)
            . flip G.runGitHub' (Just token)
            . runExceptT
            . flip runStateT state
            . flip runReaderT env
            . unSyncM

data SyncEnv = SyncEnv
  { -- | Channel to signal the outside about state change
    statePushChan  :: Chan SyncState
  , manager        :: HTTP.Manager
  , githubHost     :: BaseUrl
  , githubToken    :: G.AuthToken

    -- | For syncing
  , syncDir        :: P.FilePath
  , syncPathMapper :: S.PathMapper
  }

instance Ser.Serialize T.Text where
  put = Ser.put . T.encodeUtf8
  get = Ser.get >>= either (fail . show) return . T.decodeUtf8'

instance Ser.Serialize UTCTime where
  put = Ser.put . (realToFrac :: NominalDiffTime -> Double) . utcTimeToPOSIXSeconds
  get = posixSecondsToUTCTime . (realToFrac :: Double -> NominalDiffTime) <$> Ser.get

instance Ser.Serialize P.FilePath where
  put = Ser.put . either id id . P.toText
  get = P.fromText <$> Ser.get

instance H.HashAlgorithm a => Ser.Serialize (H.Digest a) where
  put = Ser.put . BA.unpack
  get = do
    bs :: B.ByteString <- Ser.get
    maybe (fail "Unable to unserialize hash digest") return $
      H.digestFromByteString bs

deriving instance Generic (S.SyncFile a)
instance H.HashAlgorithm a => Ser.Serialize (S.SyncFile a)

-- TODO: validate its consistency on program start (when decoded from disk)
newtype SyncState = SyncState
  { syncFiles :: M.Map P.FilePath SyncFile'
  } deriving (Show, Eq, Generic)

instance Ser.Serialize SyncState

instance Monoid SyncState where
  mempty = SyncState M.empty
  s1 `mappend` s2 = SyncState $ syncFiles s1 `mappend` syncFiles s2

instance MonadState SyncState SyncM where
  get = SyncM get
  put v = do
    chan <- asks statePushChan
    -- notify the new state
    liftIO $ writeChan chan v
    SyncM $ put v

liftGitHub :: G.GitHub a -> SyncM a
liftGitHub = SyncM . lift . lift . lift

-- | Perform a sync step and spit out a list of actions User should apply
--   discretion and filter/transform the plans as needed before actually
--   applying them
genSyncPlans :: SyncM [SyncPlan']
genSyncPlans = do
  env <- ask
  -- get all the files under the syncDir
  fs <- flip Ttl.fold Fold.list $ do
    f <- Ttl.ls (syncDir env)
    Ttl.testfile f >>= guard
    return f
  -- merge with the existing sync files
  syncFs <- gets syncFiles
  let fs' = M.elems $ (Right <$> syncFs) <> M.fromList [ (f, Left f) | f <- fs ]
  infos <- forM fs' $ \f -> do
    let p = either id S.syncFilePath f
    modT <- Ttl.modificationTime <$> Ttl.lstat p
    h <- H.hash <$> liftIO (B.readFile $ P.encodeString p)
    return S.LocalFileInfo{ S.localFileHash=h
                          , S.localFileLastModified=posixSecondsToUTCTime modT
                          }
  let fsWithInfos = zip fs' infos
  gists <- liftGitHub $ G.gists Nothing
  let actionEths = S.computeSyncActions (syncPathMapper env) fsWithInfos gists
  forM actionEths $ either (throwError . SyncLogicError) pure

-- | Actually perform the actions, updating the internal sync state
--   as a result
performSyncActions :: UTCTime -> [SyncAction'] -> SyncM ()
performSyncActions time acts = do
  newSFiles <- mapM handle acts
  modify $ \st -> st{ syncFiles = foldr (\f -> M.insert (S.syncFilePath f) f) (syncFiles st) newSFiles  }
  where
    handle x@S.UpdateLocal{} =
      write (S.localFilePath x) (S.remoteFileURL x) (S.remoteGistFileId x)
    handle x@S.CreateLocal{} =
      write (S.localFilePath x) (S.remoteFileURL x) (S.remoteGistFileId x)
    handle x@S.UpdateRemote{} =
      upload False (S.localFilePath x) (S.localFileInfo x) (S.remoteGistFileId x)
    handle x@S.CreateRemote{} =
      upload True (S.localFilePath x) (S.localFileInfo x) (S.remoteGistFileId x)
    write :: P.FilePath -> T.Text -> S.GistFileId -> SyncM SyncFile'
    write f url gfid = do
      mgr <- asks manager
      -- XXX: more efficient to set up a pipe to dump to file directly
      liftIO $ do
        req <- HTTP.parseUrl (T.unpack url)
        content <- HTTP.responseBody <$> HTTP.httpLbs req mgr
        BL.writeFile (P.encodeString f) content
        return S.SyncFile
          { S.syncFilePath = f
          , S.syncGistFileId = gfid
          , S.syncFileHash = H.hash (BL.toStrict content)
          , S.syncFileTime = time
          }
    upload
      :: Bool
      -> P.FilePath
      -> S.LocalFileInfo H.MD5
      -> S.GistFileId
      -> SyncM SyncFile'
    upload isNew f S.LocalFileInfo{S.localFileHash} gfid@(gid,fid) = do
      -- XXX: avoid reading file repeatedly?
      newContent <- liftIO . T.readFile $ P.encodeString f
      _ <- liftGitHub $ api newContent
      return S.SyncFile
        { S.syncFilePath = f
        , S.syncGistFileId = gfid
        , S.syncFileHash = localFileHash
        , S.syncFileTime = time
        }
      where
        api content
          | isNew = G.createGist $ GC.createFile fid mempty{ GC.content = content }
          | otherwise = G.editGist gid $ GE.editFile fid mempty{ GE.content = Just content }
