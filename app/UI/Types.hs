{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module UI.Types
  (
    AppState(..)
  , LogLvl(..)
  , LogMsg(..)
  , Name
  , RunMode(..)
  , AppConfig(..)
  , Choice(..)
  , UserChoice
  , AppWorkingArea(..)
  , AppMsg(..)

  , areaLockedToCurrentWork
  ) where

import qualified Brick as Bk
import           Control.Applicative
import           Control.Concurrent
import           Data.Aeson
import           Data.Monoid
import qualified Data.Sequence as Seq
import           Data.String
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import qualified Filesystem.Path.CurrentOS as P
import           GHC.Generics
import qualified Graphics.Vty as V
import qualified Network.GitHub as G
import qualified Servant.Client as Servant

import qualified SyncState as SS
import qualified SyncStrategy as SStrat

-- | the UI state
data AppState = AppState
  {
    appConfig :: AppConfig
  -- ^ needed for rendering, but should never change during app run
  , appActionHistory :: Seq.Seq (Time.UTCTime, SS.SyncAction')

  , appLogs :: Seq.Seq LogMsg
  -- ^ currently a bounded queue in memory, but can dump to somewhere if needed
  , appMsgQueue :: Seq.Seq AppMsg
  -- ^ accumulated msgs while we are dealing with something else
  , appWorkingArea :: AppWorkingArea
  -- ^ the current item waiting for a response
  }

data LogLvl = Log | Warn | Error deriving (Show, Eq, Enum, Bounded)
data LogMsg = LogMsg LogLvl T.Text deriving (Show)

type Name = ()

data RunMode = Normal | Dry
  deriving (Show, Read, Eq, Enum, Bounded, Generic)

instance FromJSON RunMode
instance ToJSON RunMode

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

  -- debug
  , runMode        :: RunMode
  }

instance FromJSON AppConfig where
  parseJSON (Object v)  = AppConfig
    <$> (P.fromText <$> v .: "sync-state-storage")
    <*> ((v .: "github-host" >>= parseURL) <|> return defGithubHost)
    <*> (fromString <$> v .: "github-token")
    <*> (P.fromText <$> v .: "sync-dir")
    <*> (parseInterval <$> v .: "sync-interval")
    <*> ((v .: "sync-strategy" >>= parseStrat) <|> return mempty)
    <*> (v .: "run-mode" <|> return Normal)
    where defGithubHost = Servant.BaseUrl Servant.Https "api.github.com" 443 ""
          parseURL t = either (fail . show) return $ Servant.parseBaseUrl t
          parseInterval :: Double -> Time.NominalDiffTime
          parseInterval = realToFrac
          parseStrat = either (fail . show) return . SStrat.parseStrategy
  parseJSON _ = empty

-- | An arrow type that keeps a tag about itself
data Choice m a b = Choice
  { choiceTag :: m
  , runChoice :: a -> Maybe b
  } deriving (Functor)

-- user choice from a list of text choices
type UserChoice = Choice [T.Text]

instance Monoid m => Applicative (Choice m a) where
  pure = Choice mempty . pure . pure
  Choice t1 h1 <*> Choice t2 h2 = Choice (t1 <> t2) ((<*>) <$> h1 <*> h2)

instance Monoid m => Alternative (Choice m a) where
  empty = Choice mempty (pure empty)
  Choice t1 h1 <|> Choice t2 h2 = Choice (t1 <> t2) ((<|>) <$> h1 <*> h2)

data AppWorkingArea = SyncPlansResolveConflict
                      { originalPlans    :: [SS.SyncPlan']
                      , pendingActions   :: [SS.SyncAction']
                      , pendingConflicts :: [SS.SyncConflict']

                      , currentConflict  :: SS.SyncConflict'
                      -- ^ the current conflict to resolve with user
                      , userChoice       :: UserChoice V.Event
                                            (Bk.EventM Name AppWorkingArea)
                      -- ^ user options to resolve the conflict
                      , replyMVar        :: MVar [SS.SyncAction']
                      }
                    | SyncPlansWaitPerform
                      { originalPlans     :: [SS.SyncPlan']
                      , performingActions :: [SS.SyncAction']
                      }
                    | DisplayMsg
                      { displayMsg :: LogMsg
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
areaLockedToCurrentWork DisplayMsg{} = False
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
