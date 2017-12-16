{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module App.Text
  (
    runApp
  ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent
import           Control.Monad (forever, unless)
import           Data.Char (toLower, toUpper)
import           Data.Either (partitionEithers)
import           Data.List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as TB
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Format as Time
import           Data.Vinyl (rvalf)
import qualified Filesystem.Path.CurrentOS as P
import           Formatting
import           Formatting.Internal (Format(..))
import           System.IO (hFlush, stdout, stderr)
import qualified Turtle as Ttl

import qualified Network.GitHub.Gist.Sync as S
import qualified Network.GitHub.Types.Gist as G

import qualified App.Core as Core
import           App.Types.Core
import qualified SyncState as SS
import qualified SyncStrategy as SStrat

runApp
  :: AppConfig
  -> SS.SyncEnv
  -> SS.SyncState
  -> IO ()
runApp conf syncEnv syncState0 = do
  msgChan <- newChan
  _ <- forkIO $ Core.runStateBackupWorker
       (rvalf #sync_state_storage conf) (SS.statePushChan syncEnv) (writeChan msgChan)
  _ <- forkIO $ Core.runSyncWorker
       (rvalf #sync_interval conf) syncEnv syncState0 (writeChan msgChan)

  forever $ readChan msgChan >>= handleMsg conf

handleMsg :: AppConfig -> AppMsg -> IO ()
handleMsg conf msg = case msg of
  MsgSyncPlansPending{..} -> case msgPendingPlans of
    [] -> putMVar msgMVar [] -- immediately return
    plans -> do
      printPlans '-' "Plans (original)" plans
      let strat = valueOf $ rvalf #sync_strategy conf
          applied = SStrat.applyStrategyToList strat plans
      printPlans '-' "Plans (filtered)" applied
      -- check if there are conflicts to resolve
      let (conflicts, acts) = partitionEithers applied
      resolved <- resolve conflicts
      let acts' = sort $ acts++resolved
      printPlans '#' "Executing actions" (Right <$> acts')
      putMVar msgMVar acts'
    -- ignore cases where we have no plans
      where
        selectStrat strats = do
          fprint ("Please select "%stext%": ") choices
          hFlush stdout
          Ttl.readline >>= \case
            Nothing -> noresp
            Just line
              | t <- Ttl.lineToText line
              , Just (x, _) <- T.uncons t -> maybe wrong return $
                lookup (toLower x) strats <|> lookup (toUpper x) strats
              | otherwise -> noresp
          where toChoice (c, strat) = sformat accentuate (c,nameOf strat)
                choices = T.intercalate "/" $ map toChoice strats
                noresp = T.putStr "Please choose an option. " >> selectStrat strats
                wrong = T.putStr "Unrecognized option. " >> selectStrat strats
        resolve' [] acc = return acc
        resolve' (c:cs) acc = do
          fprint ("Resolving: " % conflict % "\n") c
          strat <- selectStrat Core.defaultConflictResolveStrategies
          case SStrat.applyStrategy (valueOf strat) (Left c) of
            Just (Left c') -> resolve' (c':cs) acc
            Just (Right a) -> resolve' cs (a:acc)
            _ -> resolve' cs acc
        resolve [] = return []
        resolve cs = do
          fprint (headingL '!') "Conflicts detected"
          resolve' cs []
  MsgSyncActionsPerformed _ actions -> unless (null actions) $
    fprint ("Executed "%int%" actions.\n") (length actions)
  MsgSyncStatePersisted _ ->
    T.putStrLn "Sync state persisted."
  MsgSyncWorkerError err ->
    hprint stderr ("ERROR|sync-worker: "%shown%"\n") err
  MsgSyncWorkerDied err ->
    hprint stderr ("ERROR|sync-worker: Worker died. "%shown%"\n") err

heading :: Char -> Format r (Text -> r)
heading char = mapf (\t -> " "<>t<>" ") (center 75 char)

headingL :: Char -> Format r (Text -> r)
headingL char = heading char % "\n"

path :: Format r (P.FilePath -> r)
path = mapf (either id id . P.toText) stext

timeF :: String -> Format r (UTCTime -> r)
timeF format = mapf (Time.formatTime Time.defaultTimeLocale format) string

timeIso :: Format r (UTCTime -> r)
timeIso = timeF "%m-%dT%H:%M"

fid :: Format r (G.FileId -> r)
fid = mapf (\x -> "["<>x<>"]") stext

-- ignore gist id for now
gfid :: Format r (S.GistFileId -> r)
gfid = mapf snd fid

conflict :: Format r (SS.SyncConflict' -> r)
conflict = mapf S.conflictLocalFilePath path
        <> "@" % mapf (S.localFileLastModified . S.conflictLocalFileInfo) timeIso
        <> fconst " <> "
        <> mapf S.conflictRemoteGistFileId gfid
        <> "@" % mapf S.conflictRemoteUpdateTime timeIso

choose :: Format r (a -> r) -> Format r (b -> r) -> Format r (Either a b -> r)
choose a b = Format (\k -> either (runFormat a k) (runFormat b k))

action :: Format r (SS.SyncAction' -> r)
action = later toB
  where
    toB S.UpdateLocal{..}  = bprint (path % " <- " % gfid) localFilePath remoteGistFileId
    toB S.CreateLocal{..}  = bprint (path % "* <- " % gfid) localFilePath remoteGistFileId
    toB S.DeleteLocal{..}  = bprint (path % "X <- " % fid) localFilePath remoteFileId
    toB S.UpdateRemote{..} = bprint (path % " -> " % gfid) localFilePath remoteGistFileId
    toB S.CreateRemote{..} = bprint (path % " -> *" % fid) localFilePath remoteFileId
    toB S.DeleteRemote{..} = bprint (path % " -> X" % gfid) localFilePath remoteGistFileId

accentuate :: Format r ((Char, Text) -> r)
accentuate = later $ TB.fromText . toT
  where toT (c, t) | T.null aft = "["<>T.singleton c<>"]"<>t
                   | otherwise = bef <> "["<>T.singleton c<>"]" <> T.tail aft
          where (bef, aft) = T.break (==c) t

printPlans :: Char -> Text -> [SS.SyncPlan'] -> IO ()
printPlans headingChar header ps = do
  fprint (headingL headingChar) header
  case ps of
    [] -> putStrLn "No items."
    _ -> mapM_ (fprint $ choose conflict action % "\n") (sort ps)
