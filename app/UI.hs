{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Module used for parsing options
module UI
  ( app

  , appConfigFromYaml
  , bootstrapAppState
  ) where

import qualified Brick as Bk
import qualified Brick.BChan as Bk
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import qualified Data.ByteString as B
import qualified Data.Sequence as Seq
import qualified Data.Serialize as Ser
import qualified Data.Time.Clock as Time
import qualified Data.Yaml as Y
import qualified Filesystem.Path.CurrentOS as P
import qualified Graphics.Vty as V
import qualified Network.GitHub.Gist.Sync as S
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified SyncState as SS
import           System.IO

import           UI.Types

app :: Bk.App AppState AppMsg Name
app = Bk.App
  { Bk.appDraw = drawUI
  , appChooseCursor = Bk.neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = _
  }

runStateBackupWorker
  :: P.FilePath
  -> Chan SS.SyncState
  -> Bk.BChan AppMsg
  -> IO ()
runStateBackupWorker outputPath stateUpdChan msgChan = forever $ do
  st <- readChan stateUpdChan

  B.writeFile (P.encodeString outputPath) (Ser.encode st)
  Bk.writeBChan msgChan $ SyncStatePersisted st

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
bootstrapAppState :: AppConfig -> IO AppState
bootstrapAppState conf@AppConfig{..} = do
  -- communication chans
  appMsgChan <- Bk.newBChan 10
  sStateChan <- newChan

  -- initial state
  mgr <- HTTP.newManager HTTP.tlsManagerSettings
  let syncEnv = SS.SyncEnv
        { statePushChan = sStateChan
        , manager = mgr
        , githubHost = githubHost
        , githubToken = githubToken
        , syncDir = syncDir
        , syncPathMapper = defaultSyncPathMapper syncDir
        }
  syncState0 <- decodeState `catch` \(e :: SomeException) -> do
    hPutStrLn stderr $
      "Unable to load sync state, err: " ++ show e
      ++ "\nFalling back to default state..."
    return mempty

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
    { appConfig = conf
    , appActionHistory = mempty
    , appPendingActions = mempty
    , appPendingConflicts = mempty
    , appLogs = mempty
    , appMsgQueue = mempty
    , appWorkingArea = mempty
    }
  where
    decodeState = do
      bs <- B.readFile (P.encodeString syncStateStorage)
      either fail return (Ser.decode bs)

-- continually execute pending items in the state until stop
fillWorkingArea :: MonadIO m => AppState -> m AppState
fillWorkingArea st@AppState{appWorkingArea=NoWork, appMsgQueue}
  | next Seq.:< rest <- Seq.viewl appMsgQueue = _ -- TODO: deal with it
  | otherwise = return st -- all done!
fillWorkingArea st
  -- working area already filled
  = return st

-- this typically involves looking at the working area and do things as needed
handleVtyEvent :: AppState -> V.Event -> Bk.EventM Name (Bk.Next AppState)
handleVtyEvent = _

handleEvent :: AppState -> Bk.BrickEvent Name AppMsg -> Bk.EventM Name (Bk.Next AppState)
handleEvent st (Bk.AppEvent msg)
  = fillWorkingArea st{ appMsgQueue = msg Seq.<| appMsgQueue st } >>= Bk.continue
handleEvent st (Bk.VtyEvent evt)
  = handleVtyEvent st evt
handleEvent st _
  -- ignore mouse events
  = Bk.continue st

drawUI :: AppState -> [Bk.Widget Name]
drawUI st = _
