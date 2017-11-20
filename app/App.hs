{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | Module used for parsing options
module App
  ( app

  , appConfigFromYaml
  , bootstrapAppState
  ) where

import qualified Brick as Bk
import qualified Brick.BChan as Bk
import qualified Brick.Widgets.Dialog as Bk
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import qualified Data.ByteString as B
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Serialize as Ser
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import qualified Data.Yaml as Y
import qualified Debug.Trace as Trace
import qualified Filesystem.Path.CurrentOS as P
import qualified Graphics.Vty as V
import qualified Network.GitHub.Gist.Sync as S
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified SyncState as SS
import           System.IO

import           App.Types
import           App.UI (drawUI, getAttrMap)
import qualified SyncStrategy as SStrat

app :: Bk.App AppState AppMsg Name
app = Bk.App
  { Bk.appDraw = drawUI
  , appChooseCursor = Bk.neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = getAttrMap
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
      liftIO $ do
        Bk.writeBChan msgChan $ SyncActionsPerformed filteredActions
        -- wait for said time
        threadDelay $ ceiling (interval * 1e6)

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
    , appLogs = mempty
    , appMsgQueue = mempty
    , appWorkingArea = mempty
    }
  where
    decodeState = do
      bs <- B.readFile (P.encodeString syncStateStorage)
      either fail return (Ser.decode bs)

defaultConflictResolveStrategies :: [(String, SStrat.SyncStrategy)]
defaultConflictResolveStrategies =
  [ ("useNewer" , SStrat.useNewerForConflict)
  , ("useLocal" , SStrat.useLocalForConflict)
  , ("useRemote", SStrat.useRemoteForConflict)
  , ("Ignore"   , SStrat.ignoreConflicts)
  ]

applyWaitPerform
  :: MonadIO m
  => [SS.SyncPlan']
  -> MVar [SS.SyncAction']
  -> [SS.SyncAction']
  -> AppState
  -> m AppState
applyWaitPerform originalPlans msgMVar performingActions st = do
  -- immediately write the results throw to reply and then
  liftIO $ putMVar msgMVar performingActions
  return st
    { appWorkingArea = SyncPlansWaitPerform{..}
    }

applySyncPlans
  :: MonadIO m
  => [SS.SyncPlan']
  -> MVar [SS.SyncAction']
  -> AppState
  -> m AppState
applySyncPlans plans msgMVar st@AppState{appConfig}
  -- apply the default strategy
  | Just (conflictHead, more) <- conflictsMay = return st
      { appWorkingArea = SyncPlansResolveConflict
          { originalPlans = plans
          , pendingActions = actions
          , pendingConflicts = more
          , currentConflict = conflictHead
          , strategyChoice = Bk.dialog Nothing
                             (Just (0, defaultConflictResolveStrategies)) 80
          , replyMVar = msgMVar
          }
      }
  | otherwise = applyWaitPerform plans msgMVar actions st
  where
    plans' = SStrat.applyStrategyToList (syncStrategy appConfig) plans
    actions = sort $ rights plans'
    conflictsMay = uncons $ lefts plans'

applyMsg :: LogMsg -> AppState -> AppState
applyMsg msg st = Trace.traceShow msg st
  { appLogs = appLogs st Seq.|> msg
  , appWorkingArea = DisplayMsg msg
  }

-- continually execute pending items in the state until stop
processMsgQueue :: MonadIO m => AppState -> m AppState
processMsgQueue st@AppState{appWorkingArea, appMsgQueue, appActionHistory}
  | areaLockedToCurrentWork appWorkingArea = return st
  | next Seq.:< rest <- Seq.viewl appMsgQueue = case next of
      SyncPlansPending plans msgMVar ->
        applySyncPlans plans msgMVar st{ appMsgQueue = rest }
      SyncActionsPerformed actions -> do
        now <- liftIO Time.getCurrentTime
        return . flip applyMsg st
                   { appActionHistory = appActionHistory Seq.><
                                        Seq.fromList ((now,) <$> actions) }
               . LogMsg Log $ "Synced! Actions: " <> T.pack (show actions)
      SyncStatePersisted _ ->
        -- ignoring this message for the moment
        return st
      SyncWorkerError err ->
        return . flip applyMsg st . LogMsg Error $
        "SyncWorker: " <> T.pack (show err)
      SyncWorkerDied err ->
        return . flip applyMsg st . LogMsg Error $
        "SyncWorker died! " <> T.pack (show err)
  | otherwise = return st -- no more messages!

-- this typically involves looking at the working area and do things as needed
handleVtyEvent :: AppState -> V.Event -> Bk.EventM Name (Bk.Next AppState)
-- global events are handled first
handleVtyEvent st (V.EvKey (V.KChar 'q') []) = Bk.halt st
handleVtyEvent st@AppState{appWorkingArea} evt
  | SyncPlansResolveConflict{ strategyChoice, .. } <- appWorkingArea =
      case evt of
        V.EvKey V.KEnter []
          | Just strat <- Bk.dialogSelection strategyChoice ->
              case SStrat.applyStrategy strat (Left currentConflict) of
                Just (Right act) -> handleAct (Just act)
                Just (Left conflict') -> Bk.continue $ st
                  { appWorkingArea = appWorkingArea
                    { currentConflict = conflict' }
                  }
                -- conflict is ignored
                Nothing -> handleAct Nothing
              where
                handleAct actMay
                  | conflict':more' <- pendingConflicts = Bk.continue st
                    { appWorkingArea = appWorkingArea
                      { pendingActions = acts'
                      , pendingConflicts = more'
                      , currentConflict = conflict'
                      }
                    }
                  | otherwise = applyWaitPerform originalPlans replyMVar acts' st
                            >>= Bk.continue
                  where acts' = pendingActions++maybeToList actMay
        _ -> do
          d' <- Bk.handleDialogEvent evt strategyChoice
          Bk.continue st{ appWorkingArea = appWorkingArea{ strategyChoice = d' } }

  | not (areaLockedToCurrentWork appWorkingArea) = case evt of
      -- dismiss working area
      V.EvKey V.KEnter [] -> Bk.continue st{ appWorkingArea = NoWork }
      _ -> Bk.continue st

  | otherwise = Bk.continue st

handleEvent :: AppState -> Bk.BrickEvent Name AppMsg -> Bk.EventM Name (Bk.Next AppState)
handleEvent st (Bk.AppEvent msg)
  = processMsgQueue st{ appMsgQueue = msg Seq.<| appMsgQueue st } >>= Bk.continue
handleEvent st (Bk.VtyEvent evt)
  = handleVtyEvent st evt
handleEvent st _
  -- ignore mouse events
  = Bk.continue st
