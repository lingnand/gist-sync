module App.Types
  (
    AppState(..)
  , Timestamped
  , LogLvl(..)
  , LogMsg(..)
  , Name
  , AppWorkingArea(..)
  , AppMsg(..)

  , areaLockedToCurrentWork
  , module App.Types.Config
  ) where

import qualified Brick.Widgets.Dialog as Bk
import           Control.Concurrent
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Time.Clock as Time

import qualified SyncState as SS
import qualified SyncStrategy as SStrat

import           App.Types.Config

-- | the UI state
type Timestamped a = (Time.UTCTime, a)

data AppState = AppState
  {
    appConfig :: AppConfig
  -- ^ needed for rendering, but should never change during app run
  , appActionHistory :: Seq.Seq (Timestamped SS.SyncAction')

  , appLogs :: Seq.Seq (Timestamped LogMsg)
  -- ^ currently a bounded queue in memory, but can dump to somewhere if needed
  , appMsgQueue :: Seq.Seq AppMsg
  -- ^ accumulated msgs while we are dealing with something else
  , appWorkingArea :: AppWorkingArea
  -- ^ the current item waiting for a response
  }

data LogLvl = Log | Warn | Error deriving (Show, Eq, Enum, Bounded)
data LogMsg = LogMsg
  { logLvl :: LogLvl
  , logMsg :: T.Text
  } deriving (Show, Eq)

type Name = ()

data AppWorkingArea = SyncPlansResolveConflict
                      { originalPlans    :: [SS.SyncPlan']
                      , pendingActions   :: [SS.SyncAction']
                      , pendingConflicts :: [SS.SyncConflict']

                      , currentConflict  :: SS.SyncConflict'
                      -- ^ the current conflict to resolve with user
                      , strategyChoice   :: Bk.Dialog SStrat.SyncStrategy
                      -- ^ user options to resolve the conflict
                      , replyMVar        :: MVar [SS.SyncAction']
                      }
                    | SyncPlansWaitPerform
                      { originalPlans     :: [SS.SyncPlan']
                      , performingActions :: [SS.SyncAction']
                      }
                    | AlertMsg
                      { alertMsg :: LogMsg
                      }
                    | NoWork -- nothing outstanding

instance Monoid AppWorkingArea where
  mempty = NoWork
  a `mappend` NoWork = a
  NoWork `mappend` a = a
  x `mappend` _ = x

-- | Determines whether the working area can be open for use on new task
areaLockedToCurrentWork :: AppWorkingArea -> Bool
areaLockedToCurrentWork NoWork = False
areaLockedToCurrentWork AlertMsg{} = True
areaLockedToCurrentWork SyncPlansWaitPerform{} = True
areaLockedToCurrentWork SyncPlansResolveConflict{} = True

data AppMsg = SyncPlansPending
              { pendingPlans :: [SS.SyncPlan']
              , msgMVar      :: MVar [SS.SyncAction']
              -- ^ where transformed actions are written to
              }
            | SyncActionsPerformed
              { performedActions :: [SS.SyncAction']
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
