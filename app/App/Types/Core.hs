module App.Types.Core
  (
    AppMsg(..)
  , module App.Types.Config
  ) where


import           Control.Concurrent
import           Control.Monad.Catch (SomeException)

import qualified SyncState as SS

import           App.Types.Config

data AppMsg = MsgSyncPlansPending
              { msgPendingPlans :: [SS.SyncPlan']
              , msgMVar         :: MVar [SS.SyncAction']
              -- ^ where transformed actions are written to
              }
            | MsgSyncActionsPerformed
              { msgOriginalPlans    :: [SS.SyncPlan']
              , msgPerformedActions :: [SS.SyncAction']
              }
            | MsgSyncStatePersisted
              { msgNewSyncState :: SS.SyncState
              }
            | MsgSyncWorkerError
              { msgSyncWorkerError :: SomeException
              }
            | MsgSyncWorkerDied
              { msgSyncWorkerError :: SomeException
              }
