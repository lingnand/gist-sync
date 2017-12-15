{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import           Control.Applicative ((<|>), Alternative(..))
import           Control.Concurrent
import           Data.Default
import           Data.Semigroup
import qualified Data.Text as T
import           Data.Vinyl
  ( rgetf, (<<$>>), (<<*>>), ElField(..), ElField, (:::), Rec(..)
  , rcast, rtraverse)
import           Data.Vinyl.Functor (Const(..))
import qualified Filesystem.Path.CurrentOS as P
import qualified Options.Applicative as O
import qualified Turtle as Ttl

import qualified App.Brick.App as BkApp
import qualified App.Core as Core
import           App.Types.Core (Config)
import qualified App.Types.Core as Core
import           Utils ((:.), Lift(..), Compose(..))

type Opts = ("config" ::: P.FilePath) ': Config
type CLIOpts f = Rec (f :. ElField) Opts

cliOptsParser
  :: forall f. Alternative f => O.Parser (CLIOpts f)
cliOptsParser =
  rtraverse getCompose $
    parseMapper <<$>> Core.configLabels <<*>>
    ( Lift (Compose . pure . Field . P.fromText . T.pack . getConst)
      :& Core.configParser
    )
  where
    pack3 = Compose . fmap Compose
    parseMapper
      :: Const String x  -- label name
      -> Lift (->)
           (Core.FieldParser O.ReadM)
           -- ^ parser function
           (O.Parser :. f :. ElField)
           -- ^ return the option parser, f is used to store empty value when
           -- flag unspecified (all flags are optional)
           x
    parseMapper clabel = Lift $ \parser -> pack3 $
          pure <$> O.option (O.str >>= Core.runFieldParser parser) (O.long label)
      <|> pure empty
      where label = getConst clabel

runApp :: IO ()
runApp =  do
  opts <- Ttl.options "The Gist synchronization client" cliOptsParser
  confFromFile <- either (return . const def)
     (\(Field f) ->
        either (error . (("Cannot load config file "++show f++": ")++) . show) id
        <$> Core.appConfigFromYaml f)
     (getCompose $ rgetf #config opts)
  conf <- either (fail . ("Config error: "++)) return
        . rtraverse getCompose $ rcast opts
                              <> confFromFile
                              <> def

  putStrLn "== Conf =="
  print conf

  -- communication chans
  sStateChan <- newChan
  (syncEnv, syncState0) <- Core.initSyncStates sStateChan conf

  BkApp.runApp conf syncEnv syncState0


main :: IO ()
main = runApp
{-# INLINABLE main #-}

foreign export ccall runApp :: IO ()
