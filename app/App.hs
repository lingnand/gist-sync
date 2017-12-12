{-# LANGUAGE OverloadedLabels #-}
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
import qualified Data.Time.Format as Time
import           Data.Vinyl (rvalf)
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
  Bk.writeBChan msgChan $ MsgSyncStatePersisted st

runSyncWorker
  :: Time.NominalDiffTime
  -> Bk.BChan AppMsg
  -> SS.SyncEnv
  -> SS.SyncState
  -> IO ()
runSyncWorker interval msgChan syncEnv syncState0 = do
  result <- SS.runSyncM runM syncEnv syncState0
  case result of
    Left err -> Bk.writeBChan msgChan $ MsgSyncWorkerDied err
    Right (_, finalState) -> do
      Bk.writeBChan msgChan . MsgSyncWorkerDied . SS.OtherException . SomeException $
        userError "Impossible error: worker finished with final state!"
      -- backup just in case
      writeChan (SS.statePushChan syncEnv) finalState
  where
    runM = forever $
      -- catch error in each run, note this doesn't prevent from IOException
      -- terminating the thread
      oneRun `catchError` \e ->
        liftIO . Bk.writeBChan msgChan $ MsgSyncWorkerError e
        -- ignoring it and continue...
      where
        oneRun = do
          -- immediately do a sync
          now <- liftIO Time.getCurrentTime
          plans <- SS.genSyncPlans
          filteredActions <- liftIO $ do
            respMVar <- newEmptyMVar
            liftIO . Bk.writeBChan msgChan $ MsgSyncPlansPending plans respMVar
            takeMVar respMVar
          SS.performSyncActions now filteredActions
          liftIO $ do
            Bk.writeBChan msgChan $ MsgSyncActionsPerformed plans filteredActions
            -- wait for said time
            threadDelay $ ceiling (interval * 1e6)

defaultSyncPathMapper :: P.FilePath -> S.PathMapper
defaultSyncPathMapper syncDir = S.pathMapper pathToId idToPath
  where pathToId = either id id . P.toText . P.filename
        idToPath = (syncDir P.</>) . P.fromText

appConfigFromYaml
  :: Alternative f
  => P.FilePath -> IO (Either Y.ParseException (PartialAppConfig f))
appConfigFromYaml = Y.decodeFileEither . P.encodeString

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
        , githubHost = rvalf #github_host conf
        , githubToken = rvalf #github_token conf
        , syncDir = syncDir
        , syncPathMapper = defaultSyncPathMapper syncDir
        }
        where syncDir = rvalf #sync_dir conf
  syncState0 <- decodeState `catch` \(e :: SomeException) -> do
    hPutStrLn stderr $
      "Unable to load sync state, err: " ++ show e
      ++ "\nFalling back to default state..."
    return mempty

  return ((syncEnv, syncState0), appState)
  where
    decodeState = do
      bs <- B.readFile (P.encodeString $ rvalf #sync_state_storage conf)
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
applyWaitPerform areaOriginalPlans msgMVar areaPerformingActions st = do
  -- immediately write the results throw to reply and then
  liftIO $ putMVar msgMVar areaPerformingActions
  return st
    { appWorkingArea = AreaSyncPlansWaitPerform{..}
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
      { appWorkingArea = AreaSyncPlansResolveConflict
          { areaOriginalPlans = sortedPlans
          , areaPendingActions = actions
          , areaPendingConflicts = more
          , areaCurrentConflict = conflictHead
          , areaStrategyChoice = Bk.dialog Nothing
                             (Just (0, defaultConflictResolveStrategies)) 80
          , areaReplyMVar = msgMVar
          }
      }
  | otherwise = applyWaitPerform sortedPlans msgMVar actions st
  where
    sortedPlans = sort plans
    plans' = SStrat.applyStrategyToList (valueOf $ rvalf #sync_strategy appConfig) sortedPlans
    actions = rights plans'
    conflictsMay = uncons $ lefts plans'

applyMsg :: MonadIO m => LogMsg -> AppState -> m AppState
applyMsg msg st = do
  now <- liftIO Time.getCurrentTime
  return $ traceShowWithT now msg $ st
    { appLogs = appLogs st Seq.|> (now, msg)
    , appWorkingArea = area
    }
  where area | logLvl msg == Error = AreaAlertMsg msg
             | otherwise           = appWorkingArea st

-- continually execute pending items in the state until stop
processMsgQueue :: MonadIO m => AppState -> m AppState
processMsgQueue st@AppState{appWorkingArea, appMsgQueue, appActionHistory}
  | areaLockedToCurrentWork appWorkingArea = return st
  | next Seq.:< rest <- Seq.viewl appMsgQueue =
    let st' = st{ appMsgQueue = rest }
    in processMsgQueue =<< case next of
      MsgSyncPlansPending plans msgMVar ->
        applySyncPlans plans msgMVar st'
      MsgSyncActionsPerformed plans actions -> do
        now <- liftIO Time.getCurrentTime
        let tracer | null actions = id
                   | otherwise = traceShowWithT now actions
        tracer$ applyMsg
          (LogMsg Log $ "Synced! Performed "
           <> (T.pack . show . length $ actions) <> " actions")
          st' { appActionHistory = appActionHistory Seq.><
                                  Seq.fromList ((now,) <$> actions)
              , appWorkingArea = AreaSyncActionsPerformed
                                { areaOriginalPlans = plans
                                , areaPerformedActions = actions }
              }
      MsgSyncStatePersisted _ ->
        -- ignoring this message for the moment
        return st'
      MsgSyncWorkerError err ->
        flip applyMsg st' . LogMsg Error $ "SyncWorker: " <> T.pack (show err)
      MsgSyncWorkerDied err ->
        flip applyMsg st' . LogMsg Error $ "SyncWorker died! " <> T.pack (show err)
  | otherwise = return st -- no more messages!

-- FIXME: conflict resolvement dialog box doesn't seem to be working
-- this typically involves looking at the working area and do things as needed
handleVtyEvent :: AppState -> V.Event -> Bk.EventM Name (Bk.Next AppState)
-- global events are handled first
handleVtyEvent st (V.EvKey (V.KChar 'q') []) = Bk.halt st
handleVtyEvent st@AppState{appWorkingArea} evt = case appWorkingArea of
  AreaNoWork -> Bk.continue st
  AreaSyncPlansResolveConflict{ areaStrategyChoice, .. }
    | V.EvKey V.KEnter [] <- evt
    , Just strat <- Bk.dialogSelection areaStrategyChoice ->
      case SStrat.applyStrategy strat (Left areaCurrentConflict) of
        Just (Right act) -> handleAct (Just act)
        Just (Left conflict') -> Bk.continue $ st
          { appWorkingArea = appWorkingArea
            { areaCurrentConflict = conflict' }
          }
        -- conflict is ignored
        Nothing -> handleAct Nothing
    | otherwise -> do
        d' <- Bk.handleDialogEvent evt areaStrategyChoice
        Bk.continue st{ appWorkingArea = appWorkingArea{ areaStrategyChoice = d' } }
      where
        handleAct actMay
          | conflict':more' <- areaPendingConflicts = Bk.continue st
            { appWorkingArea = appWorkingArea
              { areaPendingActions = acts'
              , areaPendingConflicts = more'
              , areaCurrentConflict = conflict'
              }
            }
          | otherwise = applyWaitPerform areaOriginalPlans areaReplyMVar acts' st
                    >>= Bk.continue
          where acts' = areaPendingActions++maybeToList actMay

  AreaAlertMsg{} -> case evt of
      -- dismiss working area
      V.EvKey V.KEnter [] -> Bk.continue st{ appWorkingArea = AreaNoWork }
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

-- debug

traceShowWithT :: Show a => Time.UTCTime -> a -> b -> b
traceShowWithT t i = Trace.trace (fmtT++" "++show i)
  where fmtT = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S" t
