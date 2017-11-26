{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module App.Types
  (
    AppState(..)
  , Timestamped
  , LogLvl(..)
  , LogMsg(..)
  , Name
  , RunMode(..)
  , AppWorkingArea(..)
  , AppMsg(..)

  , areaLockedToCurrentWork

  , AppConfig'(..)
  , AppConfig
  , syncStateStorage
  , githubHost
  , githubToken
  , syncDir
  , syncInterval
  , syncStrategy
  , runMode
  , hoistConfig
  , evalConfig'
  , evalConfig
  , Parser(runParser)
  , hoistParser
  , configParser
  , defConfig
  ) where

import qualified Brick.Widgets.Dialog as Bk
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.Except
import           Data.Aeson
import           Data.Char
import           Data.Functor.Compose
import           Data.Functor.Identity
import qualified Data.Sequence as Seq
import           Data.String
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import qualified Filesystem.Path.CurrentOS as P
import           GHC.Generics
import qualified Network.GitHub as G
import qualified Servant.Client as Servant
import           Text.Read (readEither)

import qualified SyncState as SS
import qualified SyncStrategy as SStrat

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

data RunMode = Normal | Dry
  deriving (Show, Read, Eq, Enum, Bounded, Generic)

instance FromJSON RunMode
instance ToJSON RunMode


data AppConfig' f = AppConfig'
  { _syncStateStorage :: f P.FilePath
  -- ^ path to a file that is used to read/write sync state
  -- copied from SyncEnv
  , _githubHost       :: f Servant.BaseUrl
  , _githubToken      :: f G.AuthToken
  , _syncDir          :: f P.FilePath
  , _syncInterval     :: f Time.NominalDiffTime
  , _syncStrategy     :: f SStrat.SyncStrategy
  -- ^ strategy to apply after each sync

  -- debug
  , _runMode          :: f RunMode
  }

type AppConfig = AppConfig' Identity

-- | A Monoid implementation that merges values for individual fields
instance Alternative f => Monoid (AppConfig' f) where
  mempty = AppConfig'
    { _syncStateStorage  = empty
    , _githubHost        = empty
    , _githubToken       = empty
    , _syncDir           = empty
    , _syncInterval      = empty
    , _syncStrategy      = empty
    , _runMode           = empty
    }
  c1 `mappend` c2 = AppConfig'
    { _syncStateStorage  = _syncStateStorage c1 <|> _syncStateStorage c2
    , _githubHost        = _githubHost c1 <|> _githubHost c2
    , _githubToken       = _githubToken c1 <|> _githubToken c2
    , _syncDir           = _syncDir c1 <|> _syncDir c2
    , _syncInterval      = _syncInterval c1 <|> _syncInterval c2
    , _syncStrategy      = _syncStrategy c1 <|> _syncStrategy c2
    , _runMode           = _runMode c1 <|> _runMode c2
    }

syncStateStorage :: AppConfig -> P.FilePath
syncStateStorage = runIdentity . _syncStateStorage
githubHost :: AppConfig -> Servant.BaseUrl
githubHost = runIdentity . _githubHost
githubToken :: AppConfig -> G.AuthToken
githubToken = runIdentity . _githubToken
syncDir :: AppConfig -> P.FilePath
syncDir = runIdentity . _syncDir
syncInterval :: AppConfig -> Time.NominalDiffTime
syncInterval = runIdentity . _syncInterval
syncStrategy :: AppConfig -> SStrat.SyncStrategy
syncStrategy = runIdentity . _syncStrategy
runMode :: AppConfig -> RunMode
runMode = runIdentity . _runMode

hoistConfig :: (forall a. f a -> g a) -> AppConfig' f -> AppConfig' g
hoistConfig f AppConfig'{..} = AppConfig'
  { _syncStateStorage  = f _syncStateStorage
  , _githubHost        = f _githubHost
  , _githubToken       = f _githubToken
  , _syncDir           = f _syncDir
  , _syncInterval      = f _syncInterval
  , _syncStrategy      = f _syncStrategy
  , _runMode           = f _runMode
  }

evalConfig'
  :: Applicative g
  => (forall a. f a -> g (h a))
  -> AppConfig' f -> g (AppConfig' h)
evalConfig' f AppConfig'{..} = AppConfig'
  <$> f _syncStateStorage
  <*> f _githubHost
  <*> f _githubToken
  <*> f _syncDir
  <*> f _syncInterval
  <*> f _syncStrategy
  <*> f _runMode

evalConfig :: Applicative f => AppConfig' f -> f AppConfig
evalConfig = evalConfig' (fmap pure)

type family ParserInput a :: * where
  ParserInput P.FilePath = T.Text
  ParserInput Servant.BaseUrl = String
  ParserInput G.AuthToken = String
  ParserInput Time.NominalDiffTime = Double
  ParserInput SStrat.SyncStrategy = String
  ParserInput RunMode = String

newtype Parser f a = Parser{ runParser :: ParserInput a -> f a }

hoistParser :: (f a -> g a) -> Parser f a -> Parser g a
hoistParser f (Parser x) = Parser $ f . x

configParser
  :: MonadError String f
  => AppConfig' (Parser f)
configParser = AppConfig'
  { _syncStateStorage = Parser $ pure . P.fromText
  , _githubHost       = Parser $ either (throwError . show) pure
                               . Servant.parseBaseUrl
  , _githubToken      = Parser $ pure . fromString
  , _syncDir          = Parser $ pure . P.fromText
  , _syncInterval     = Parser $ pure . realToFrac
  , _syncStrategy     = Parser $ either (throwError . show) pure
                               . SStrat.parseStrategy
  , _runMode          = Parser $ either throwError pure . tryCases
  }
  where
    tryCases [] = throwError "Cannot parse empty string"
    tryCases s@(x:xs) = readEither s <|> readEither (toUpper x:xs)

defConfig
  :: MonadError String f
  => AppConfig' f
defConfig = AppConfig'
  { _syncStateStorage = throwError "No sync state storage specified"
  , _githubHost       = pure $ Servant.BaseUrl Servant.Https "api.github.com" 443 ""
  , _githubToken      = throwError "No github token specified"
  , _syncDir          = throwError "No sync dir specified"
  , _syncInterval     = throwError "No sync interval specified"
  , _syncStrategy     = throwError "No sync strategy specified"
  , _runMode          = pure Normal
  }

instance Alternative f => FromJSON (AppConfig' f) where
  parseJSON (Object v) = evalConfig' getCompose parse
    where AppConfig'{..} = configParser
          conv p = either fail return . runParser p
          up x = Compose (pure <$> x <|> pure empty)
          parse = AppConfig'
            { _syncStateStorage = up $ v .: "sync-state-storage" >>= conv _syncStateStorage
            , _githubHost       = up $ v .: "github-host" >>= conv _githubHost
            , _githubToken      = up $ v .: "github-token" >>= conv _githubToken
            , _syncDir          = up $ v .: "sync-dir" >>= conv _syncDir
            , _syncInterval     = up $ v .: "sync-interval" >>= conv _syncInterval
            , _syncStrategy     = up $ v .: "sync-strategy" >>= conv _syncStrategy
            , _runMode          = up $ v .: "run-mode" >>= conv _runMode
            }
  parseJSON _ = empty

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
