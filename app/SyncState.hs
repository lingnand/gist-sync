{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Control.Foldl as Fold
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Crypto.Hash as H
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
import qualified Data.Time.Clock as Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Filesystem.Path.CurrentOS as P
import           GHC.Generics
import qualified Network.GitHub as G
import qualified Network.GitHub.Gist.Sync as S
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
               | SyncActionNotImplemented SyncAction'
               | OtherException SomeException
               deriving Show

instance Exception SyncError

newtype SyncM a = SyncM
  { unSyncM :: ReaderT SyncEnv (StateT SyncState G.GitHub) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader SyncEnv
           , MonadThrow, MonadCatch )

runSyncM
  :: SyncM a
  -> SyncEnv
  -> SyncState
  -> IO (a, SyncState)
runSyncM m env@SyncEnv{githubToken=token, manager=mgr, githubHost=host} state =
  run m >>= either (throwM . ServantError) return
  where run = flip runClientM (ClientEnv mgr host)
            . flip G.runGitHub' (Just token)
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
liftGitHub = SyncM . lift . lift

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
    Ttl.testfile p >>= \case
      False -> return Nothing
      True -> do
        modT <- Ttl.modificationTime <$> Ttl.lstat p
        h <- H.hash <$> liftIO (B.readFile $ P.encodeString p)
        return $ Just S.LocalFileExist
          { S.localFileHash=h
          , S.localFileLastModified=posixSecondsToUTCTime modT
          }
  let fsWithInfos = zip fs' infos
  gists <- liftGitHub $ G.gists Nothing
  let actionEths = S.computeSyncActions (syncPathMapper env) fsWithInfos gists
  forM actionEths $ either (throwM . SyncLogicError) pure

-- | Actually perform the actions, updating the internal sync state
--   as a result
performSyncActions :: UTCTime -> [SyncAction'] -> SyncM ()
performSyncActions _ [] = return ()
performSyncActions time acts = do
  newSFiles <- mapM handle acts
  modify $ \st -> st{ syncFiles = foldr (\f -> M.insert (S.syncFilePath f) f) (syncFiles st) newSFiles  }
  where
    handle S.UpdateLocal{..} =
      write localFilePath remoteFileURL remoteGistFileId
    handle S.CreateLocal{..} =
      write localFilePath remoteFileURL remoteGistFileId
    handle S.UpdateRemote{..} =
      upload localFilePath localFileInfo $ \content -> do
        let gfid@(gid,fid) = remoteGistFileId
        _ <- G.editGist gid $ S.editFile fid
             mempty{ S.fileEditContent = Just content }
        return gfid
    handle S.CreateRemote{..} =
      upload localFilePath localFileInfo $ \content -> do
        g <- G.createGist $ S.createFile remoteFileId
             mempty{ S.fileCreateContent = content }
        return (S.gistId g, remoteFileId)
    handle act@S.DeleteLocal{} =
      throwM $ SyncActionNotImplemented act
    handle act@S.DeleteRemote{} =
      throwM $ SyncActionNotImplemented act
    write :: P.FilePath -> T.Text -> S.GistFileId -> SyncM SyncFile'
    write f url gfid = do
      mgr <- asks manager
      -- XXX: more efficient to set up a pipe to dump to file directly
      liftIO $ do
        req <- HTTP.parseRequest (T.unpack url)
        content <- HTTP.responseBody <$> HTTP.httpLbs req mgr
        BL.writeFile (P.encodeString f) content
        return S.SyncFile
          { S.syncFilePath = f
          , S.syncGistFileId = gfid
          , S.syncFileHash = H.hash (BL.toStrict content)
          , S.syncFileTime = time
          -- ^ this would be later than the remote update time
          }
    upload
      :: P.FilePath
      -> S.LocalFileExist H.MD5
      -> (T.Text -> G.GitHub S.GistFileId)
      -> SyncM SyncFile'
    upload f S.LocalFileExist{S.localFileHash} api = do
      -- XXX: avoid reading file repeatedly?
      newContent <- liftIO . T.readFile $ P.encodeString f
      gfid <- liftGitHub $ api newContent
      -- get a new timestamp that's later than the remote update time
      newTime <- liftIO Time.getCurrentTime
      return S.SyncFile
        { S.syncFilePath = f
        , S.syncGistFileId = gfid
        , S.syncFileHash = localFileHash
        , S.syncFileTime = newTime
        }
