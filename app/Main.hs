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
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Default
import           Data.Maybe
import           Data.Semigroup
import           Data.Vinyl (Rec(..), (<<*>>), rget, rcast, rtraverse)
import qualified Filesystem.Path.CurrentOS as P
import qualified Graphics.Vty as V
import qualified Options.Applicative as O
import qualified Turtle as Ttl

import           App
  (Attr', Attr(..), SLabel(..), ConfLabels, ParserInput(..), ParserInput', rattr)

import qualified App
import           Utils ((:.), Lift(..), Compose(..))

appMsgLimit :: Int
appMsgLimit = 5

data ConfigPathL

type instance Attr' ConfigPathL = P.FilePath
data instance SLabel ConfigPathL = SConfigPathL
-- type instance ParserInput' ConfigPathL = T.Text

type CLIConfLabels = ConfigPathL ': ConfLabels
type CLIOpts f = Rec (f :. Attr) CLIConfLabels

cliOptsParser
  :: forall f. Alternative f => O.Parser (CLIOpts f)
cliOptsParser =
  rtraverse getCompose $
       pack3 (pure . Attr <$> Ttl.optPath "config" 'c' "Configuration path"
              <|> pure empty)
    :& parse <<*>> App.configParser
  where
    pack3 = Compose . fmap Compose
    up
      :: ((ParserInput' a -> O.ReadM (Attr a))-> O.Parser (Attr a))
      -> Lift (->)
         (Lift (->) ParserInput (O.ReadM :. Attr))
         (O.Parser :. f :. Attr) a
    up opt = Lift $ \(Lift parser) -> pack3 $
      -- fall back to 'empty' if option not specified
      pure <$> opt (getCompose . parser . ParserInput) <|> pure empty
    parse
      :: Alternative f
      => Rec (Lift (->)
              (Lift (->) ParserInput (O.ReadM :. Attr))
              -- ^ parser function
              (O.Parser :. f :. Attr))
              -- ^ return the option parser, f is used to store empty value when
              -- flag unspecified (all flags are optional)
              ConfLabels
    parse =
         up (\f -> O.option (O.str >>= f)
              (O.long "sync-state-storage" <> O.short 'S'))
      :& up (\f -> O.option (O.str >>= f)
              (O.long "github-host" <> O.short 'G'))
      :& up (\f -> O.option (O.str >>= f)
              (O.long "github-token" <> O.short 'T'))
      :& up (\f -> O.option (O.str >>= f)
              (O.long "sync-dir" <> O.short 'D'))
      :& up (\f -> O.option (O.auto >>= f)
              (O.long "sync-interval" <> O.short 'i'))
      :& up (\f -> O.option (O.str >>= f)
              (O.long "sync-strategy"))
      :& up (\f -> O.option (O.str >>= f)
              (O.long "run-mode" <> O.short 'm'))
      :& RNil

runApp :: IO ()
runApp =  do
  opts <- Ttl.options "The Gist synchronization client" cliOptsParser
  confFromFile <- either (return . const def)
     (\(Attr f) -> fromMaybe (error $ "Cannot load config file "++show f)
            <$> App.appConfigFromYaml f)
     (getCompose $ rget SConfigPathL opts)
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
       (rattr SSyncStateStorageL conf) sStateChan appMsgChan
  _ <- forkIO $ App.runSyncWorker
       (rattr SSyncIntervalL conf) appMsgChan syncEnv syncState0

  void $ Bk.customMain (V.mkVty V.defaultConfig) (Just appMsgChan) App.app appState


main :: IO ()
main = runApp
{-# INLINABLE main #-}

foreign export ccall runApp :: IO ()
