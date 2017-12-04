{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Types.Config
  (
    RunMode(..)
  , Named(..)

  , SyncStateStorageL
  , GitHubHostL
  , GitHubTokenL
  , SyncDirL
  , SyncIntervalL
  , SyncStrategyL
  , RunModeL

  , SLabel(..)
  , Attr'
  , Attr(..)
  , (=:)
  , rattr

  , ConfLabels

  , AppConfig
  , PartialAppConfig

  , ParserInput'
  , ParserInput(..)

  , configParser
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Aeson (FromJSON, (.:?))
import qualified Data.Aeson.Types as A
import           Data.Char
import           Data.Default
import           Data.String
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import           Data.Vinyl (Rec(..), (<<*>>), (<<$>>), rtraverse, rget, RElem)
import           Data.Vinyl.Functor (Lift(..))
import qualified Filesystem.Path.CurrentOS as P
import qualified Network.GitHub as G
import qualified Servant.Client as Servant
import qualified SyncStrategy as SStrat
import           Text.Read (readEither)
import           Utils (Compose(..), (:.))
import qualified Utils as U

-- declaring labels of kind * to allow easy extension
data SyncStateStorageL
data GitHubHostL
data GitHubTokenL
data SyncDirL
data SyncIntervalL
data SyncStrategyL
data RunModeL

-- hand-rolled singleton mirrors
data family SLabel a
data instance SLabel SyncStateStorageL = SSyncStateStorageL
data instance SLabel GitHubHostL       = SGitHubHostL
data instance SLabel GitHubTokenL      = SGitHubTokenL
data instance SLabel SyncDirL          = SSyncDirL
data instance SLabel SyncIntervalL     = SSyncIntervalL
data instance SLabel SyncStrategyL     = SSyncStrategyL
data instance SLabel RunModeL          = SRunModeL

data RunMode = Normal | Dry
  deriving (Show, Read, Eq, Enum, Bounded)

-- wrapper that allows storing a name
data Named a = Named
  { nameOf  :: T.Text
  , valueOf :: a
  }

-- Attr type family and newtype wrapper
-- NOTE: we are using type family instead of GADT to hold the data fields
-- directly because of the convenience this has over GADT on unpacking the data
-- using accessor
type family Attr' a
type instance Attr' SyncStateStorageL = P.FilePath
type instance Attr' GitHubHostL       = Servant.BaseUrl
type instance Attr' GitHubTokenL      = G.AuthToken
type instance Attr' SyncDirL          = P.FilePath
type instance Attr' SyncIntervalL     = Time.NominalDiffTime
type instance Attr' SyncStrategyL     = Named SStrat.SyncStrategy
type instance Attr' RunModeL          = RunMode
newtype Attr a = Attr { unAttr :: Attr' a }
infixl 8 =:
(=:) :: sing a -> Attr' a -> Attr a
_ =: x = Attr x

rattr :: RElem u us i => sing u -> Rec Attr us -> Attr' u
rattr s = unAttr . rget s

instance Show (Attr SyncStateStorageL) where
  show (Attr p) = "syncStateStorage = "++show p
instance Show (Attr GitHubHostL) where
  show (Attr h) = "githubHost = "++show h
instance Show (Attr GitHubTokenL) where
  show (Attr _) = "githubToken = <token>"
instance Show (Attr SyncDirL) where
  show (Attr d) = "syncDir = "++show d
instance Show (Attr SyncIntervalL) where
  show (Attr i) = "syncInterval = "++show i
instance Show (Attr SyncStrategyL) where
  show (Attr s) = "syncStrategy = "++T.unpack (nameOf s)
instance Show (Attr RunModeL) where
  show (Attr m) = "runMode = "++show m

type ConfLabels = '[ SyncStateStorageL
                   , GitHubHostL
                   , GitHubTokenL
                   , SyncDirL
                   , SyncIntervalL
                   , SyncStrategyL
                   , RunModeL
                   ]

type AppConfig = Rec Attr ConfLabels
type PartialAppConfig f = Rec (f :. Attr) ConfLabels

-- type family to compute parser input type + wrapper around it
type family ParserInput' a
type instance ParserInput' SyncStateStorageL = T.Text
type instance ParserInput' GitHubHostL       = String
type instance ParserInput' GitHubTokenL      = String
type instance ParserInput' SyncDirL          = T.Text
type instance ParserInput' SyncIntervalL     = Double
type instance ParserInput' SyncStrategyL     = String
type instance ParserInput' RunModeL          = String
newtype ParserInput a = ParserInput { unParserInput :: ParserInput' a }

configParser
  :: Monad f -- MonadFail in later ghc
  => Rec (Lift (->) ParserInput (f :. Attr)) ConfLabels
configParser =
     up ( pure . (SSyncStateStorageL =:) . P.fromText )
  :& up ( either (fail . show)
            (pure . (SGitHubHostL =:))
          . Servant.parseBaseUrl )
  :& up (pure . (SGitHubTokenL =:) . fromString)
  :& up (pure . (SSyncDirL =:) . P.fromText)
  :& up (pure . (SSyncIntervalL =:) . realToFrac)
  :& up (\str ->
            either (fail . show)
            (pure . (SSyncStrategyL =:) . Named (T.pack str))
            (SStrat.parseStrategy str))
  :& up (either fail (pure . (SRunModeL =:)) . tryCases)
  :& RNil
  where
    up f = Lift $ Compose . f . unParserInput
    tryCases [] = Left "Cannot parse empty string"
    tryCases s@(x:xs) = readEither s <|> readEither (toUpper x:xs)

instance MonadError String f => Default (PartialAppConfig f) where
  def =
       up (throwError "No sync state storage specified")
    :& up (pure $ SGitHubHostL =: Servant.BaseUrl Servant.Https "api.github.com" 443 "")
    :& up (throwError "No github token specified")
    :& up (throwError "No sync dir specified")
    :& up (throwError "No sync interval specified")
    :& up (throwError "No sync strategy specified")
    :& up (pure $ SRunModeL =: Normal)
    :& RNil
    where up = Compose

instance Alternative f => FromJSON (PartialAppConfig f) where
  parseJSON (A.Object v) = do
    -- first get all the parser input from json (allowing partial input)
    inputs <- rtraverse getCompose $
           up (v .:? "sync-state-storage")
        :& up (v .:? "github-host")
        :& up (v .:? "github-token")
        :& up (v .:? "sync-dir")
        :& up (v .:? "sync-interval")
        :& up (v .:? "sync-strategy")
        :& up (v .:? "run-mode" )
        :& RNil
    let applied :: Rec (Maybe :. A.Parser :. Attr) ConfLabels
        applied = U.fmapLift <<$>> configParser <<*>> inputs
        -- shift Maybe into f
        eval :: (Maybe :. A.Parser :. Attr) a -> A.Parser ((f :. Attr) a)
        eval = on . getCompose
          where
            on Nothing = pure (Compose empty) -- unspecified field
            on (Just parse) = Compose . pure <$> getCompose parse
    -- this step will catch all the parser error right here
    rtraverse eval applied
    where
      up parser = Compose $ Compose . fmap ParserInput <$> parser
  parseJSON _ = empty
