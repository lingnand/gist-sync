{-# LANGUAGE FlexibleContexts #-}
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
  , initStates

  , runSyncWorker
  , runStateBackupWorker

  , module App.Types
  ) where

import qualified Brick as Bk
import qualified Brick.BChan as Bk
import qualified Brick.Widgets.Dialog as Bk
import           Control.Applicative
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
  -> SS.SyncEnv
  -> SS.SyncState
  -> IO ()
runSyncWorker interval msgChan syncEnv syncState0 = do
  result <- SS.runSyncM runM syncEnv syncState0
  case result of
    Left err -> Bk.writeBChan msgChan $ SyncWorkerDied err
    Right (_, finalState) -> do
      Bk.writeBChan msgChan . SyncWorkerDied . SS.OtherException . SomeException $
        userError "Impossible error: worker finished with final state!"
      -- backup just in case
      writeChan (SS.statePushChan syncEnv) finalState
  where
    runM = forever $
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

appConfigFromYaml
  :: Alternative f
  => P.FilePath -> IO (Maybe (AppConfig' f))
appConfigFromYaml = Y.decodeFile . P.encodeString

-- | init the various env/states
initStates
  :: Chan SS.SyncState -- state communication chan
  -> AppConfig
  -> IO ((SS.SyncEnv, SS.SyncState), AppState)
initStates sStateChan conf = do
  -- initial state
  mgr <- HTTP.newManager HTTP.tlsManagerSettings
  let syncEnv = SS.SyncEnv
        { statePushChan = sStateChan
        , manager = mgr
        , githubHost = githubHost conf
        , githubToken = githubToken conf
        , syncDir = syncDir conf
        , syncPathMapper = defaultSyncPathMapper (syncDir conf)
        }
  syncState0 <- decodeState `catch` \(e :: SomeException) -> do
    hPutStrLn stderr $
      "Unable to load sync state, err: " ++ show e
      ++ "\nFalling back to default state..."
    return mempty

  return ((syncEnv, syncState0), appState)
  where
    decodeState = do
      bs <- B.readFile (P.encodeString $ syncStateStorage conf)
      either fail return (Ser.decode bs)
    appState = AppState
      { appConfig = conf
      , appActionHistory = mempty
      , appLogs = mempty
      , appMsgQueue = mempty
      , appWorkingArea = mempty
      }

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
          { originalPlans = sortedPlans
          , pendingActions = actions
          , pendingConflicts = more
          , currentConflict = conflictHead
          , strategyChoice = Bk.dialog Nothing
                             (Just (0, defaultConflictResolveStrategies)) 80
          , replyMVar = msgMVar
          }
      }
  | otherwise = applyWaitPerform sortedPlans msgMVar actions st
  where
    sortedPlans = sort plans
    plans' = SStrat.applyStrategyToList (syncStrategy appConfig) sortedPlans
    actions = rights plans'
    conflictsMay = uncons $ lefts plans'

applyMsg :: MonadIO m => LogMsg -> AppState -> m AppState
applyMsg msg st = Trace.traceShow msg $ do
  now <- liftIO Time.getCurrentTime
  return st
    { appLogs = appLogs st Seq.|> (now, msg)
    , appWorkingArea = area
    }
  where area | logLvl msg == Error = AlertMsg msg
             | otherwise           = appWorkingArea st

-- continually execute pending items in the state until stop
processMsgQueue :: MonadIO m => AppState -> m AppState
processMsgQueue st@AppState{appWorkingArea, appMsgQueue, appActionHistory}
  | areaLockedToCurrentWork appWorkingArea = return st
  | next Seq.:< rest <- Seq.viewl appMsgQueue = case next of
      SyncPlansPending plans msgMVar ->
        applySyncPlans plans msgMVar st{ appMsgQueue = rest }
      SyncActionsPerformed actions -> do
        now <- liftIO Time.getCurrentTime
        applyMsg (LogMsg Log $ "Synced! Performed "
                   <> (T.pack . show . length $ actions) <> " actions")
          st { appActionHistory = appActionHistory Seq.><
                                  Seq.fromList ((now,) <$> actions)
             }
      SyncStatePersisted _ ->
        -- ignoring this message for the moment
        return st
      SyncWorkerError err ->
        flip applyMsg st . LogMsg Error $ "SyncWorker: " <> T.pack (show err)
      SyncWorkerDied err ->
        flip applyMsg st . LogMsg Error $ "SyncWorker died! " <> T.pack (show err)
  | otherwise = return st -- no more messages!

-- this typically involves looking at the working area and do things as needed
handleVtyEvent :: AppState -> V.Event -> Bk.EventM Name (Bk.Next AppState)
-- global events are handled first
handleVtyEvent st (V.EvKey (V.KChar 'q') []) = Bk.halt st
handleVtyEvent st@AppState{appWorkingArea} evt = case appWorkingArea of
  NoWork -> Bk.continue st
  SyncPlansResolveConflict{ strategyChoice, .. }
    | V.EvKey V.KEnter [] <- evt
    , Just strat <- Bk.dialogSelection strategyChoice ->
      case SStrat.applyStrategy strat (Left currentConflict) of
        Just (Right act) -> handleAct (Just act)
        Just (Left conflict') -> Bk.continue $ st
          { appWorkingArea = appWorkingArea
            { currentConflict = conflict' }
          }
        -- conflict is ignored
        Nothing -> handleAct Nothing
    | otherwise -> do
        d' <- Bk.handleDialogEvent evt strategyChoice
        Bk.continue st{ appWorkingArea = appWorkingArea{ strategyChoice = d' } }
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

  AlertMsg{} -> case evt of
      -- dismiss working area
      V.EvKey V.KEnter [] -> Bk.continue st{ appWorkingArea = NoWork }
      _ -> Bk.continue st

  _ -> Bk.continue st

handleEvent :: AppState -> Bk.BrickEvent Name AppMsg -> Bk.EventM Name (Bk.Next AppState)
handleEvent st (Bk.AppEvent msg)
  = processMsgQueue st{ appMsgQueue = msg Seq.<| appMsgQueue st } >>= Bk.continue
handleEvent st (Bk.VtyEvent evt)
  = handleVtyEvent st evt
handleEvent st _
  -- ignore mouse events
  = Bk.continue st
