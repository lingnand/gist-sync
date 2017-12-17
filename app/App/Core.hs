{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module App.Core
  (
    appConfigFromYaml
  , initSyncStates

  , runSyncWorker
  , runStateBackupWorker

  , defaultSyncPathMapper
  , defaultConflictResolveStrategies
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.Serialize as Ser
import qualified Data.Time.Clock as Time
import           Data.Vinyl (rvalf)
import qualified Data.Yaml as Y
import qualified Filesystem.Path.CurrentOS as P
import qualified Network.GitHub.Gist.Sync as S
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified SyncState as SS
import           System.IO

import           App.Types.Core
import qualified SyncStrategy as SStrat


-- NOTE: the msgSender handler needs to be fast and non-blocking, otherwise we
-- can run into trouble
-- might as well use a channel on the api? (and then create a forwarder in Brick
-- to forward msgs into BChan)

runStateBackupWorker
  :: P.FilePath
  -> Chan SS.SyncState
  -> (AppMsg -> IO ())
  -> IO ()
runStateBackupWorker outputPath stateUpdChan msgSender = forever $ do
  st <- readChan stateUpdChan
  B.writeFile (P.encodeString outputPath) (Ser.encode st)
  msgSender $ MsgSyncStatePersisted st

runSyncWorker
  :: Time.NominalDiffTime
  -> SS.SyncEnv
  -> SS.SyncState
  -> (AppMsg -> IO ())
  -> IO ()
runSyncWorker interval syncEnv syncState0 msgSender = do
  (_, finalState) <- SS.runSyncM runM syncEnv syncState0
  msgSender . MsgSyncWorkerDied . SomeException $
    userError "Impossible error: worker finished with final state!"
  -- backup just in case
  writeChan (SS.statePushChan syncEnv) finalState
  where
    runM = forever $ do
      -- catch error in each run, note this doesn't prevent from IOException
      -- terminating the thread
      oneRun `catch` \e ->
        -- TODO: catch only some exceptions and continue on.
        -- For others, we should die here and deliver a blocking message
        liftIO . msgSender $ MsgSyncWorkerError e
        -- ignoring it and continue...
      -- wait for said time
      liftIO . threadDelay $ ceiling (interval * 1e6)
      where
        oneRun = do
          -- immediately do a sync
          now <- liftIO Time.getCurrentTime
          plans <- SS.genSyncPlans
          filteredActions <- liftIO $ do
            respMVar <- newEmptyMVar
            liftIO . msgSender $ MsgSyncPlansPending plans respMVar
            takeMVar respMVar
          SS.performSyncActions now filteredActions
          liftIO . msgSender $ MsgSyncActionsPerformed plans filteredActions

defaultSyncPathMapper :: P.FilePath -> S.PathMapper
defaultSyncPathMapper syncDir = S.pathMapper pathToId idToPath
  where pathToId = either id id . P.toText . P.filename
        idToPath = (syncDir P.</>) . P.fromText

appConfigFromYaml
  :: Alternative f
  => P.FilePath -> IO (Either Y.ParseException (PartialAppConfig f))
appConfigFromYaml = Y.decodeFileEither . P.encodeString

-- | init the various env/states
initSyncStates
  :: AppConfig
  -> IO (SS.SyncEnv, SS.SyncState)
initSyncStates conf = do
  -- state communication chan
  sStateChan <- newChan
  -- initial state
  mgr <- HTTP.newManager HTTP.tlsManagerSettings
  let syncEnv = SS.SyncEnv
        { SS.statePushChan = sStateChan
        , SS.manager = mgr
        , SS.githubHost = rvalf #github_host conf
        , SS.githubToken = rvalf #github_token conf
        , SS.syncDir = syncDir
        , SS.syncPathMapper = defaultSyncPathMapper syncDir
        }
        where syncDir = rvalf #sync_dir conf
  syncState0 <- decodeState `catch` \(e :: SomeException) -> do
    hPutStrLn stderr $
      "Unable to load sync state, err: " ++ show e
      ++ "\nFalling back to default state..."
    return mempty

  return (syncEnv, syncState0)
  where
    decodeState = do
      bs <- B.readFile (P.encodeString $ rvalf #sync_state_storage conf)
      either fail return (Ser.decode bs)

-- Label to strats
defaultConflictResolveStrategies :: [(Char, Named SStrat.SyncStrategy)]
defaultConflictResolveStrategies =
  [ ('N', Named "useNewer"  SStrat.useNewerForConflict)
  , ('L', Named "useLocal"  SStrat.useLocalForConflict)
  , ('R', Named "useRemote" SStrat.useRemoteForConflict)
  , ('I', Named "Ignore"    SStrat.ignoreConflicts)
  ]
