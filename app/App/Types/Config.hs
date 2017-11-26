{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Types.Config
  (
    RunMode(..)
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

import           Control.Applicative
import           Control.Monad.Except
import           Data.Aeson
import           Data.Char
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.String
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import qualified Filesystem.Path.CurrentOS as P
import qualified Network.GitHub as G
import qualified Servant.Client as Servant
import qualified SyncStrategy as SStrat
import           Text.Read (readEither)

data RunMode = Normal | Dry
  deriving (Show, Read, Eq, Enum, Bounded)

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
hoistConfig f = runIdentity . evalConfig' (Identity . f)

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
