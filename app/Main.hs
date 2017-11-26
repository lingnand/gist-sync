{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import qualified Brick as Bk
import qualified Brick.BChan as Bk
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Functor.Compose
import           Data.Maybe
import           Data.Monoid
import qualified Filesystem.Path.CurrentOS as P
import qualified Graphics.Vty as V
import qualified Options.Applicative as O
import qualified Turtle as Ttl

import qualified App

appMsgLimit :: Int
appMsgLimit = 5

data CLIOpts f = CLIOpts
  { cliConfigPath :: f P.FilePath
  , cliConfig     :: App.AppConfig' f
  }

cliOptsParser
  :: Alternative f => O.Parser (CLIOpts f)
cliOptsParser = CLIOpts
  <$> ((pure <$> Ttl.optPath "config" 'c' "Configuration path") <|> pure empty)
  <*> App.evalConfig' getCompose parse
  where
    App.AppConfig'{..} = App.configParser
    conv f p = f >>= either fail return . App.runParser p
    up x = Compose (pure <$> x <|> pure empty)
    parse = App.AppConfig'
      { _syncStateStorage = up $ O.option (conv O.str _syncStateStorage)
                            (O.long "sync-state-storage" <> O.short 'S')
      , _githubHost       = up $ O.option (conv O.str _githubHost)
                            (O.long "github-host" <> O.short 'G')
      , _githubToken      = up $ O.option (conv O.str _githubToken)
                            (O.long "github-token" <> O.short 'T')
      , _syncDir          = up $ O.option (conv O.str _syncDir)
                            (O.long "sync-dir" <> O.short 'D')
      , _syncInterval     = up $ O.option (conv O.auto _syncInterval)
                            (O.long "sync-interval" <> O.short 'i')
      , _syncStrategy     = up $ O.option (conv O.str _syncStrategy)
                            (O.long "sync-strategy")
      , _runMode          = up $ O.option (conv O.str _runMode)
                            (O.long "run-mode" <> O.short 'm')
      }

runApp :: IO ()
runApp =  do
  opts <- Ttl.options "The Gist synchronization client" cliOptsParser
  confFromFile <- either (return . const mempty)
     (\f -> fromMaybe (error $ "Cannot load config file "++show f)
            <$> App.appConfigFromYaml f)
     (cliConfigPath opts)
  conf <- either (fail . ("Config error: "++)) return
        . App.evalConfig $ cliConfig opts
                        <> confFromFile
                        <> App.defConfig

  putStrLn $ "syncStateStorage: " ++ show (App.syncStateStorage conf)
  putStrLn $ "githubHost: " ++ show (App.githubHost conf)
  putStrLn $ "syncDir: " ++ show (App.syncDir conf)
  putStrLn $ "syncInterval: " ++ show (App.syncInterval conf)
  putStrLn $ "runMode: " ++ show (App.runMode conf)

  -- communication chans
  appMsgChan <- Bk.newBChan appMsgLimit
  sStateChan <- newChan

  -- bootstrap state
  ((syncEnv, syncState0), appState) <- App.initStates sStateChan conf

  -- workers
  _ <- forkIO $ App.runStateBackupWorker
       (App.syncStateStorage conf) sStateChan appMsgChan
  _ <- forkIO $ App.runSyncWorker
       (App.syncInterval conf) appMsgChan syncEnv syncState0

  void $ Bk.customMain (V.mkVty V.defaultConfig) (Just appMsgChan) App.app appState


main :: IO ()
main = runApp
{-# INLINABLE main #-}

foreign export ccall runApp :: IO ()
