{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Module used for parsing options
module UI
  (
  ) where

import           Control.Exception
import           Control.Monad.Except
import qualified Brick as Bk
import qualified Brick.BChan as Bk
import qualified Brick.Main as Bk
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Maybe
import           Data.String
import qualified Data.Time.Clock as Time
import qualified Data.Yaml as Y
import qualified Filesystem.Path.CurrentOS as P
import qualified Network.GitHub as G
import qualified Network.GitHub.Gist.Sync as S
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Servant.Client as Servant
import qualified SyncState as SS
import qualified SyncStrategy as SStrat

type Name = ()

data AppConfig = AppConfig
  { syncStateStorage :: P.FilePath
  -- ^ path to a file that is used to read/write sync state
  -- copied from SyncEnv
  , githubHost     :: Servant.BaseUrl
  , githubToken    :: G.AuthToken
  , syncDir        :: P.FilePath
  , syncInterval   :: Time.NominalDiffTime
  , syncStrategy   :: SStrat.SyncStrategy
  -- ^ strategy to apply after each sync
  }

instance FromJSON AppConfig where
  parseJSON (Object v)  = AppConfig
    <$> (P.fromText <$> v .: "sync-state-storage")
    <*> ((v .: "github-host" >>= parseURL) <|> return defGithubHost)
    <*> (fromString <$> v .: "github-token")
    <*> (P.fromText <$> v .: "sync-dir")
    <*> (parseInterval <$> v .: "sync-interval")
    <*> ((v .: "sync-strategy" >>= parseStrat) <|> return mempty)
    where defGithubHost = Servant.BaseUrl Servant.Https "api.github.com" 443 ""
          parseURL t = either (fail . show) return $ Servant.parseBaseUrl t
          parseInterval :: Double -> Time.NominalDiffTime
          parseInterval = realToFrac
          parseStrat = either (fail . show) return . SStrat.parseStrategy

-- | the UI state
data AppState = AppState
  {
    appConfig :: AppConfig
  -- ^ needed for rendering, but should never change during app run
  -- , undefined
  -- model side
  }

-- data App = App AppState
--   { field :: Type
--   , field :: Type
--   } deriving (Show, Eq)

data AppMsg = SyncPlansPending
              { pendingPlans :: [SS.SyncPlan']
              , respMVar       :: MVar [SS.SyncAction']
              -- ^ where transformed actions are written to
              }
            | SyncStatePersisted
              { newSyncState :: SS.SyncState
              }
            | SyncWorkerError
              { syncWorkerError :: SS.SyncError
              }
            | SyncWorkerDied
              { syncWorkerError :: SS.SyncError
              }

runStateBackupWorker
  :: P.FilePath
  -> Chan SS.SyncState
  -> Bk.BChan AppMsg
  -> IO ()
runStateBackupWorker outputPath stateUpdChan msgChan = forever $ do
  st <- readChan stateUpdChan
  serializeState st outputPath
  Bk.writeBChan msgChan $ SyncStatePersisted st
  where
    serializeState = undefined

runSyncWorker
  :: Time.NominalDiffTime
  -> Bk.BChan AppMsg
  -> SS.SyncM ()
runSyncWorker interval msgChan = forever $
  -- catch error in each run, note this doesn't prevent from IOException
  -- terminating the thread
  oneRun `catchError` \e ->
    liftIO . Bk.writeBChan msgChan $ SyncWorkerError e
    -- ignoring it and continue...
  where
    oneRun = do
      -- immediately do a sync
      now <- liftIO Time.getCurrentTime
      plans <- SS.genSyncPlans
      filteredActions <- liftIO $ do
        respMVar <- newEmptyMVar
        liftIO . Bk.writeBChan msgChan $ SyncPlansPending plans respMVar
        readMVar respMVar
      SS.performSyncActions now filteredActions
      -- wait for said time
      liftIO . threadDelay $ ceiling (interval * 1e6)

defaultSyncPathMapper :: P.FilePath -> S.PathMapper
defaultSyncPathMapper syncDir = S.pathMapper pathToId idToPath
  where pathToId = either id id . P.toText . P.filename
        idToPath = (syncDir P.</>) . P.fromText

appConfigFromYaml :: P.FilePath -> IO (Maybe AppConfig)
appConfigFromYaml = Y.decodeFile . P.encodeString

-- | start the workers and return the initial app state
bootstrapApp :: AppConfig -> IO AppState
bootstrapApp AppConfig{..} = do
  -- communication chans
  appMsgChan <- Bk.newBChan 10
  sStateChan <- newChan

  -- initial state
  sStateMay <- unserializeState syncStateStorage
  mgr <- HTTP.newManager HTTP.tlsManagerSettings
  let syncEnv = SS.SyncEnv
        { statePushChan = sStateChan
        , manager = mgr
        , githubHost = githubHost
        , githubToken = githubToken
        , syncDir = syncDir
        , syncPathMapper = defaultSyncPathMapper syncDir
        }
      syncState0 = fromMaybe mempty sStateMay

  -- workers
  _ <- forkIO $ runStateBackupWorker syncStateStorage sStateChan appMsgChan
  _ <- forkIO $ do
    result <- SS.runSyncM (runSyncWorker syncInterval appMsgChan) syncEnv syncState0
    case result of
      Left err -> Bk.writeBChan appMsgChan $ SyncWorkerDied err
      Right (_, finalState) -> do
        Bk.writeBChan appMsgChan . SyncWorkerDied . SS.OtherException . SomeException $
          userError "Impossible error: worker finished with final state!"
        -- backup just in case
        writeChan sStateChan finalState

  return AppState
    {
    }
  where
    unserializeState = undefined

drawUI :: AppState -> [Bk.Widget Name]
drawUI st = undefined

handleEvent :: AppState -> Bk.BrickEvent Name AppMsg -> Bk.EventM Name (Bk.Next AppState)
handleEvent st = undefined
