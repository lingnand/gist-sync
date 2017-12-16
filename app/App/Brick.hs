{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | Module used for parsing options
module App.Brick
  ( app
  , runApp
  ) where

import qualified Brick as Bk
import qualified Brick.BChan as Bk
import qualified Brick.Widgets.Dialog as Bk
import           Control.Concurrent
import           Control.Monad (void)
import           Control.Monad.Trans
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import           Data.Vinyl (rvalf)
import qualified Debug.Trace as Trace
import qualified Graphics.Vty as V
import qualified SyncState as SS

import           App.Brick.Types
import           App.Brick.UI (drawUI, getAttrMap)
import qualified App.Core as Core
import qualified SyncStrategy as SStrat

appMsgLimit :: Int
appMsgLimit = 5

app :: Bk.App AppState AppMsg Name
app = Bk.App
  { Bk.appDraw = drawUI
  , appChooseCursor = Bk.neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = getAttrMap
  }

runApp
  :: AppConfig
  -> SS.SyncEnv
  -> SS.SyncState
  -> IO ()
runApp conf syncEnv syncState0 = do
  appMsgChan <- Bk.newBChan appMsgLimit

  -- bootstrap state
  let appState = initAppState conf

  -- workers
  _ <- forkIO $ Core.runStateBackupWorker
       (rvalf #sync_state_storage conf) (SS.statePushChan syncEnv)
       (Bk.writeBChan appMsgChan)
  _ <- forkIO $ Core.runSyncWorker
       (rvalf #sync_interval conf) syncEnv syncState0 (Bk.writeBChan appMsgChan)

  void $ Bk.customMain (V.mkVty V.defaultConfig) (Just appMsgChan) app appState

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
            (Just (0, toChoice <$> Core.defaultConflictResolveStrategies)) 80
          , areaReplyMVar = msgMVar
          }
      }
  | otherwise = applyWaitPerform sortedPlans msgMVar actions st
  where
    toChoice (_, named) = (T.unpack $ nameOf named, valueOf named)
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
        flip applyMsg st' . LogMsg Warn $ "SyncWorker: " <> T.pack (show err)
      MsgSyncWorkerDied err ->
        flip applyMsg st' . LogMsg Error $ "SyncWorker died! " <> T.pack (show err)
  | otherwise = return st -- no more messages!

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
