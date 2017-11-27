{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module App.UI
  (
    getAttrMap
  , drawUI
  ) where

import           App.Types
import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Dialog
import           Data.Bits ((.|.))
import           Data.Either
import           Data.Foldable (toList)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.Format
import qualified Filesystem.Path.CurrentOS as P
import qualified Graphics.Vty as V
import qualified Network.GitHub.Gist.Sync as S
import qualified Network.GitHub.Types.Gist as G
import qualified SyncState as SS

textPad :: Int
textPad = 3

selectionAttr, logLabelAttr, logLabelErrorAttr, logLabelWarnAttr :: AttrName
selectionAttr     = "selection"
logLabelAttr      = "log" <> "label"
logLabelErrorAttr = logLabelAttr <> "error"
logLabelWarnAttr  = logLabelAttr <> "warn"

getAttrMap :: AppState -> AttrMap
getAttrMap _ = attrMap V.defAttr
  [ (selectionAttr, V.defAttr `V.withStyle` (V.bold .|. V.underline))
  , (logLabelAttr, V.defAttr `V.withStyle` V.bold)
  , (logLabelErrorAttr, fg V.red)
  , (logLabelWarnAttr, fg V.yellow)
  ]

drawUI :: AppState -> [Widget Name]
drawUI AppState{..} = maybeToList dialogMay ++ [mainLayer]
  where
    (dialogMay, status) = drawWorkingArea appWorkingArea
    mainLayer =
      borderWithLabel (txt "Config") (drawConfig appConfig)
      <=>
      borderWithLabel (txt "Recent actions") (drawActionHistory 5 appActionHistory)
      <=>
      borderWithLabel (txt "Log") (drawLogs 5 appLogs)
      <=>
      borderWithLabel (txt "Status") status

drawKV :: T.Text -> T.Text -> Widget n
drawKV k v = padLeftRight textPad $ txt (k <> ": " <> v)

pShowT :: P.FilePath -> T.Text
pShowT = either id id . P.toText

showT :: Show a => a -> T.Text
showT = T.pack . show

fidShowT :: G.FileId -> T.Text
fidShowT fileId = "[" <> fileId <> "]"

gfidShowT :: S.GistFileId -> T.Text
gfidShowT = fidShowT . snd

timeShowT :: String -> UTCTime -> T.Text
timeShowT format = T.pack . formatTime defaultTimeLocale format

drawConfig :: AppConfig -> Widget n
drawConfig conf =
  (hCenter (drawKV "sync-dir" (pShowT $ syncDir conf)) <+>
   hCenter (drawKV "sync-state-storage" (pShowT $ syncStateStorage conf)))
  <=>
  hBorder
  <=>
  (hCenter (drawKV "sync-strategy" (nameOf $ syncStrategy conf)) <+>
   hCenter (drawKV "sync-interval" (showT $ syncInterval conf)))

drawActionHistory
  :: Int
  -> Seq.Seq (Timestamped SS.SyncAction')
  -> Widget n
drawActionHistory = drawSequence (txt "No action history.") (txtWrap . showT)

drawLog :: LogMsg -> Widget n
drawLog (LogMsg lvl text) =
  withAttr (lAttr lvl) (txt $ showT lvl) <+> padLeft (Pad 2) (txtWrap text)
  where lAttr Error = logLabelErrorAttr
        lAttr Warn = logLabelWarnAttr
        lAttr _ = logLabelAttr

drawLogs
  :: Int
  -> Seq.Seq (Timestamped LogMsg)
  -> Widget n
drawLogs = drawSequence (txt "No logs.") drawLog

drawSequence
  :: Widget n        -- ^ empty display
  -> (a -> Widget n) -- ^ function to show
  -> Int           -- ^ number of most recent items to show
  -> Seq.Seq (Timestamped a)
  -> Widget n
drawSequence empty drawer nRecent sq
  | Seq.null sq = hCenter empty
  | otherwise =  hCenter . vBox . toList $ draw <$> items
  where items = Seq.drop (max 0 (Seq.length sq - nRecent)) sq
        draw (t, a) =  txt (timeShowT "%Y-%m-%d %H:%M:%S" t) <+> padLeft (Pad 2) (drawer a)

-- align the actions and draw them as a list of columns
drawActions :: Widget n -> [SS.SyncAction'] -> Widget n
drawActions empty [] = empty
drawActions _ as = hBox . map vBox . transpose . map widgetR $ as
  where
    r S.UpdateLocal{..}  = [pShowT localFilePath     , "<-", gfidShowT remoteGistFileId]
    r S.CreateLocal{..}  = [pShowT localFilePath<>"*", "<-", gfidShowT remoteGistFileId]
    r S.UpdateRemote{..} = [pShowT localFilePath     , "->", gfidShowT remoteGistFileId]
    r S.CreateRemote{..} = [pShowT localFilePath     , "->", fidShowT remoteFileId<>"*"]
    widgetR = map (padLeftRight textPad . txt) . r

drawConflicts :: Widget n -> [SS.SyncConflict'] -> Widget n
drawConflicts empty [] = empty
drawConflicts _ as = hBox . map vBox . transpose . map widgetR $ as
  where
    timeT = timeShowT "%m-%dT%H:%M"
    widgetR S.SyncConflict{..} = padLeftRight textPad . txt <$>
      [ pShowT conflictLocalFilePath
        <> "@"
        <> timeT (S.localFileLastModified conflictLocalFileInfo)
      , "<>"
      , gfidShowT conflictRemoteGistFileId <> "@" <> timeT conflictRemoteUpdateTime
      ]

drawPlans :: Widget n -> [SS.SyncPlan'] -> Widget n
drawPlans empty [] = empty
drawPlans _ plans = drawConflicts emptyWidget conflicts <=> drawActions emptyWidget actions
  where conflicts = lefts plans
        actions = rights plans

drawComparisonPanes :: (T.Text, Widget n) -> (T.Text, Widget n) -> Widget n
drawComparisonPanes (upLabel, up) (downLabel, down) =
  borderWithLabel (txt upLabel) (center up)
  <=>
  hBorder
  <=>
  borderWithLabel (txt downLabel) (center down)

-- | draw working area as (Maybe a top layer, status layer)
drawWorkingArea :: AppWorkingArea -> (Maybe (Widget n), Widget n)
drawWorkingArea AreaSyncPlansResolveConflict{..}
  = (Just (renderDialog areaStrategyChoice dialogBody), wStatus)
  where
    wCurrConflict = withAttr selectionAttr $ drawConflicts emptyWidget [areaCurrentConflict]
    others = drawConflicts emptyWidget areaPendingConflicts
      <=> drawActions emptyWidget areaPendingActions
    dialogBody =
      hCenter (txt "Please choose an option for current conflict")
      <=>
      drawComparisonPanes
      ("Original", drawPlans (txt "No Plans") areaOriginalPlans)
      ("Pending", wCurrConflict <=> others)
    wStatus = hCenter $ txt "Conflicts detected, resolving"
drawWorkingArea AreaSyncPlansWaitPerform{..}
  = (Nothing, wStatus)
  where
    wStatus =
      hCenter (txt "Performing actions...")
      <=>
      drawComparisonPanes
      ("Original", drawPlans (txt "No Plans") areaOriginalPlans)
      ("Actions", drawActions (txt "No Actions") areaPerformingActions)
drawWorkingArea AreaSyncActionsPerformed{..}
  = (Nothing, wStatus)
  where
    wStatus =
      hCenter (txt "Done.")
      <=>
      drawComparisonPanes
      ("Original", drawPlans (txt "No Plans") areaOriginalPlans)
      ("Actions", drawActions (txt "No Actions") areaPerformedActions)
drawWorkingArea AreaAlertMsg{..}
  = (Just (renderDialog (dialog (Just "Alert") Nothing 0) dialogBody), wStatus)
  where
    wStatus = hCenter $ txt "Alert message popped"
    dialogBody = drawLog areaAlertMsg
drawWorkingArea AreaNoWork
  = (Nothing, hCenter $ txt "Nothing to be done")
