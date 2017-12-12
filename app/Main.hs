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

import qualified Brick as Bk
import qualified Brick.BChan as Bk
import           Control.Applicative ((<|>), Alternative(..))
import           Control.Concurrent
import           Control.Monad
import           Data.Default
import           Data.Semigroup
import qualified Data.Text as T
import           Data.Vinyl
  ( rgetf, rvalf, (<<$>>), (<<*>>), ElField(..), ElField, (:::), Rec(..)
  , rcast, rtraverse)
import           Data.Vinyl.Functor (Const(..))
import qualified Filesystem.Path.CurrentOS as P
import qualified Graphics.Vty as V
import qualified Options.Applicative as O
import qualified Turtle as Ttl

import qualified App
import           Utils ((:.), Lift(..), Compose(..))

appMsgLimit :: Int
appMsgLimit = 5

type Opts = ("config" ::: P.FilePath) ': App.Config
type CLIOpts f = Rec (f :. ElField) Opts

cliOptsParser
  :: forall f. Alternative f => O.Parser (CLIOpts f)
cliOptsParser =
  rtraverse getCompose $
    parseMapper <<$>> App.configLabels <<*>>
    ( Lift (Compose . pure . Field . P.fromText . T.pack . getConst)
      :& App.configParser
    )
  where
    pack3 = Compose . fmap Compose
    parseMapper
      :: Const String x  -- label name
      -> Lift (->)
           (App.FieldParser O.ReadM)
           -- ^ parser function
           (O.Parser :. f :. ElField)
           -- ^ return the option parser, f is used to store empty value when
           -- flag unspecified (all flags are optional)
           x
    parseMapper clabel = Lift $ \parser -> pack3 $
          pure <$> O.option (O.str >>= App.runFieldParser parser) (O.long label)
      <|> pure empty
      where label = getConst clabel

runApp :: IO ()
runApp =  do
  opts <- Ttl.options "The Gist synchronization client" cliOptsParser
  confFromFile <- either (return . const def)
     (\(Field f) ->
        either (error . (("Cannot load config file "++show f++": ")++) . show) id
        <$> App.appConfigFromYaml f)
     (getCompose $ rgetf #config opts)
  conf <- either (fail . ("Config error: "++)) return
        . rtraverse getCompose $ rcast opts
                              <> confFromFile
                              <> def

  putStrLn "== Conf =="
  print conf

  -- communication chans
  appMsgChan <- Bk.newBChan appMsgLimit
  sStateChan <- newChan

  -- bootstrap state
  ((syncEnv, syncState0), appState) <- App.initStates sStateChan conf

  -- workers
  _ <- forkIO $ App.runStateBackupWorker
       (rvalf #sync_state_storage conf) sStateChan appMsgChan
  _ <- forkIO $ App.runSyncWorker
       (rvalf #sync_interval conf) appMsgChan syncEnv syncState0

  void $ Bk.customMain (V.mkVty V.defaultConfig) (Just appMsgChan) App.app appState


main :: IO ()
main = runApp
{-# INLINABLE main #-}

foreign export ccall runApp :: IO ()
