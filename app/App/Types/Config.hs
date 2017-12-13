{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TypeApplications #-}
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
{-# LANGUAGE OverloadedLabels #-}
module App.Types.Config
  (
    RunMode(..)
  , Named(..)

  , Config

  , AppConfig
  , PartialAppConfig
  , FieldParser
  , runFieldParser

  , configLabelShow
  , configLabels
  , configParser
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Monad.Except
import           Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import           Data.Aeson (FromJSON, (.:?))
import qualified Data.Aeson.Types as A
import           Data.Char
import           Data.Default
import           Data.String
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import           Data.Vinyl
  (RecApplicative,  AllFields,  Rec(..), (<<*>>), (<<$>>), rtraverse, rpure, rlabels
  , (=:), (:::), ElField, FieldRec)
import           Data.Vinyl.Functor (Lift(..), Const(..))
import qualified Filesystem.Path.CurrentOS as P
import qualified Network.GitHub as G
import qualified Servant.Client as Servant
import qualified SyncStrategy as SStrat
import           Text.Read (readEither)
import           Utils ( Compose(..), (:.) )
import qualified Utils as U

data RunMode = Normal | Dry
  deriving (Show, Read, Eq, Enum, Bounded)

-- wrapper that allows storing a name
data Named a = Named
  { nameOf  :: T.Text
  , valueOf :: a
  }

type Config = '[ "sync_state_storage" ::: P.FilePath
               , "github_host"        ::: Servant.BaseUrl
               , "github_token"       ::: G.AuthToken
               , "sync_dir"           ::: P.FilePath
               , "sync_interval"      ::: Time.NominalDiffTime
               , "sync_strategy"      ::: Named SStrat.SyncStrategy
               , "run_mode"           ::: RunMode
               ]

instance Show (Named SStrat.SyncStrategy) where
  show (Named n _) = "<"++T.unpack n++">"
instance Show G.AuthToken where
  show _ = "<auth-token>"

type AppConfig = FieldRec Config
type PartialAppConfig f = Rec (f :. ElField) Config

-- | User-facing label representation
configLabelShow :: String -> String
configLabelShow = map conv
  where conv '_' = '-'
        conv x = x

-- | The label strings used in config file/cli
configLabels
  :: (AllFields fs, RecApplicative fs) => Rec (Const String) fs
configLabels = rpure (Lift (U.over' configLabelShow)) <<*>> rlabels

type FieldParser f = Lift (->) (Const String) (f :. ElField)

mkFieldParser
  :: (String -> f (ElField x)) -> FieldParser f x
mkFieldParser parser = Lift $ Compose . parser . getConst

runFieldParser
  :: FieldParser f x -> String -> f (ElField x)
runFieldParser parser inp = getCompose wrapped
  where wrapped = getLift parser (Const inp)

configParser
  :: forall f. MonadFail f
  => Rec (FieldParser f) Config
configParser =
     mkFieldParser (pure . (#sync_state_storage =:) . P.fromText . T.pack)
  :& mkFieldParser (liftEth . fmap (#github_host =:) . Servant.parseBaseUrl)
  :& mkFieldParser (pure . (#github_token =:) . fromString)
  :& mkFieldParser (pure . (#sync_dir =:) . P.fromText . T.pack)
  :& mkFieldParser (liftEth . fmap ((#sync_interval =:) . realToFrac @Double) . readEither)
  :& mkFieldParser (\str -> liftEth $
                     (#sync_strategy =:) . Named (T.pack str)
                     <$> SStrat.parseStrategy str)
  :& mkFieldParser (liftEth . fmap (#run_mode =:) . tryCases)
  :& RNil
  where
    tryCases [] = Left "Cannot parse empty string"
    tryCases s@(x:xs) = readEither s <|> readEither (toUpper x:xs)
    liftEth :: Show a => Either a v -> f v
    liftEth = either (Fail.fail . show) pure

instance MonadError String f => Default (PartialAppConfig f) where
  def =
       up (throwError "No sync state storage specified")
    :& up (pure $ #github_host =: Servant.BaseUrl Servant.Https "api.github.com" 443 "")
    :& up (throwError "No github token specified")
    :& up (throwError "No sync dir specified")
    :& up (throwError "No sync interval specified")
    :& up (throwError "No sync strategy specified")
    :& up (pure $ #run_mode =: Normal)
    :& RNil
    where up = Compose

instance Alternative f => FromJSON (PartialAppConfig f) where
  parseJSON (A.Object v) = do
    -- first get all the parser input from json (allowing partial input)
    inputs <- rtraverse getCompose $ jsonInputParser v <<*>> configLabels
    let applied :: Rec (Maybe :. A.Parser :. ElField) Config
        applied = U.fmapLift <<$>> configParser <<*>> inputs
        -- shift Maybe into f
        eval :: (Maybe :. A.Parser :. ElField) a -> A.Parser ((f :. ElField) a)
        eval = on . getCompose
          where
            on Nothing = pure (Compose empty) -- unspecified field
            on (Just parse) = Compose . pure <$> getCompose parse
    -- this step will catch all the parser error right here
    rtraverse eval applied
    where
      jsonInputParser
        :: A.Object
        -> Rec (Lift (->) (Const String) (A.Parser :. Maybe :. Const String)) Config
      jsonInputParser v =
           strProp -- #sync_state_storage
        :& strProp -- #github_host
        :& strProp -- #github_token
        :& strProp -- #sync_dir
        :& doubleProp -- #sync_interval
        :& strProp -- #sync_strategy
        :& strProp -- #run_mode
        :& RNil
        where
          wrap f = Lift $ \(Const l) -> Compose $ Compose . fmap Const <$> f l
          strProp
            :: forall x. Lift (->) (Const String) (A.Parser :. Maybe :. Const String) x
          strProp = wrap $ \l -> v .:? T.pack l
          doubleProp = wrap $ \l -> fmap (show @Double) <$> (v .:? T.pack l)
  parseJSON _ = empty
