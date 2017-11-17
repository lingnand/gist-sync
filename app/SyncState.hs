{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module SyncState
  (
  ) where

import           Control.Concurrent.Chan
import           Control.Exception
import qualified Control.Foldl as Fold
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Crypto.Hash as H
import           Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Filesystem.Path.CurrentOS as P
import qualified Network.GitHub as G
import qualified Network.GitHub.Gist.Sync as S
import qualified Network.HTTP.Client as HTTP
import           Servant.Client (ServantError, BaseUrl, runClientM, ClientEnv(..))
import qualified Turtle as Ttl

-- import qualified SyncStrategy as SS

data SyncError = SyncLogicError T.Text
               | forall a. SyncCannotPerformAction (S.SyncAction a)
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
  a <- (bimap ServantError id <$> run m) `catch` (return . Left . OtherException)
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

data SyncState = SyncState
  { syncFiles :: M.Map P.FilePath (S.SyncFile H.MD5)
  } deriving (Show, Eq)

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
--   discretion and filter/transform the actions as needed before actually
--   applying them
genSyncActions :: SyncM [S.SyncAction H.MD5]
genSyncActions = do
  env <- ask
  -- get all the files under the syncDir
  fs <- flip Ttl.fold Fold.list $ do
    f <- Ttl.ls (syncDir env)
    Ttl.testfile f >>= guard
    return f
  -- merge with the existing sync files
  syncFs <- gets syncFiles
  let fs' = M.elems $ (Right <$> syncFs) <> M.fromList [ (f, Left f) | f <- fs ]
  liftIO . putStrLn $ "localFiles: " ++ show fs'
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

-- | Apply the sync strategy saved in the environment
--   NOTE: the result could well still contain SyncConflict, the user should
--   figure out a way to rewrite them into actionable items before calling
--   'performSyncAction'

-- | Actually perform the actions, updating the internal sync state
--   as a result
performSyncActions :: UTCTime -> [S.SyncAction H.MD5] -> SyncM ()
performSyncActions time acts = do
  newSFiles <- mapM handle acts
  modify $ \st -> st{ syncFiles = foldr (\f -> M.insert (S.syncFilePath f) f) (syncFiles st) newSFiles  }
  where
    handle x@S.UpdateLocal{} =
      write (S.localFilePath x) (S.remoteFileURL x) (S.remoteGistFileId x)
    handle x@S.CreateLocal{} =
      write (S.localFilePath x) (S.remoteFileURL x) (S.remoteGistFileId x)
    handle x@S.UpdateRemote{} =
      error "FIXME: not implemented"
    handle x@S.CreateRemote{} =
      error "FIXME: not implemented"
    handle x@S.SyncConflict{} =
      throwError $ SyncCannotPerformAction x
    write :: P.FilePath -> T.Text -> S.GistFileId -> SyncM (S.SyncFile H.MD5)
    write f url gid = do
      mgr <- asks manager
      -- XXX: more efficient to set up a pipe to dump to file directly
      liftIO $ do
        req <- HTTP.parseUrl (T.unpack url)
        content <- HTTP.responseBody <$> HTTP.httpLbs req mgr
        BL.writeFile (P.encodeString f) content
        return S.SyncFile
          { S.syncFilePath = f
          , S.syncGistFileId = gid
          , S.syncFileHash = H.hash (BL.toStrict content)
          , S.syncFileTime = time
          }
