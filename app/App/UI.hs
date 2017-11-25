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
      borderWithLabel (txt "Recent actions") (drawActionHistory 10 appActionHistory)
      <=>
      borderWithLabel (txt "Log") (drawLogs 10 appLogs)
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
drawConfig AppConfig{..} =
  hCenter (drawKV "sync-state-storage" (pShowT syncStateStorage))
  <=>
  hBorder
  <=>
  (hCenter (drawKV "sync-dir" (pShowT syncDir))
    <+>
    hCenter (drawKV "sync-interval" (showT syncInterval)))

drawActionHistory
  :: Int
  -> Seq.Seq (Timestamped SS.SyncAction')
  -> Widget n
drawActionHistory = drawSequence (txtWrap . showT)

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
drawLogs = drawSequence drawLog

drawSequence
  :: (a -> Widget n) -- ^ function to show
  -> Int           -- ^ number of most recent items to show
  -> Seq.Seq (Timestamped a)
  -> Widget n
drawSequence drawer nRecent sq = hCenter . vBox . toList $ draw <$> items
  where items = Seq.drop (max 0 (Seq.length sq - nRecent)) sq
        draw (t, a) =  txt (timeShowT "%Y-%m-%d %H:%M:%S" t) <+> padLeft (Pad 2) (drawer a)

-- align the actions and draw them as a list of columns
drawActions :: [SS.SyncAction'] -> [Widget n]
drawActions = map vBox . transpose . map widgetR
  where
    r S.UpdateLocal{..}  = [pShowT localFilePath     , "<-", gfidShowT remoteGistFileId]
    r S.CreateLocal{..}  = [pShowT localFilePath<>"*", "<-", gfidShowT remoteGistFileId]
    r S.UpdateRemote{..} = [pShowT localFilePath     , "->", gfidShowT remoteGistFileId]
    r S.CreateRemote{..} = [pShowT localFilePath     , "->", fidShowT remoteFileId<>"*"]
    widgetR = map (padLeftRight textPad . txt) . r

drawConflicts :: [SS.SyncConflict'] -> [Widget n]
drawConflicts = map vBox . transpose . map widgetR
  where
    timeT = timeShowT "%m-%dT%H:%M"
    widgetR S.SyncConflict{..} = padLeftRight textPad . txt <$>
      [ pShowT conflictLocalFilePath
        <> "@"
        <> timeT (S.localFileLastModified conflictLocalFileInfo)
      , "<>"
      , gfidShowT conflictRemoteGistFileId <> "@" <> timeT conflictRemoteUpdateTime
      ]

drawPlans :: [SS.SyncPlan'] -> [Widget n]
drawPlans plans = zipWith (<=>) (drawConflicts conflicts) (drawActions actions)
  where conflicts = lefts plans
        actions = rights plans

-- | draw working area as (Maybe a top layer, status layer)
drawWorkingArea :: AppWorkingArea -> (Maybe (Widget n), Widget n)
drawWorkingArea SyncPlansResolveConflict{..}
  = (Just (renderDialog strategyChoice dialogBody), wStatus)
  where
    wCurrConflict = map (withAttr selectionAttr) $ drawConflicts [currentConflict]
    others = zipWith (<=>) (drawConflicts pendingConflicts) (drawActions pendingActions)
    dialogBody =
      hCenter (txt "Please choose an option for current conflict")
      <=>
      (borderWithLabel (txt "Original")
       (hCenter . hBox $ drawPlans originalPlans)
       <+>
       vBorder
       <+>
       borderWithLabel (txt "Pending")
       (hCenter . hBox $ zipWith (<=>) wCurrConflict others))
    wStatus = hCenter $ txt "Conflicts detected, resolving"
drawWorkingArea SyncPlansWaitPerform{..}
  = (Nothing, wStatus)
  where
    wStatus =
      hCenter (txt "Performing actions...")
      <=>
      (borderWithLabel (txt "Original")
        (hCenter . hBox $ drawPlans originalPlans)
        <+>
        vBorder
        <+>
        borderWithLabel (txt "Actions")
        (hCenter . hBox $ drawActions performingActions))
drawWorkingArea AlertMsg{..}
  = (Just (renderDialog (dialog (Just "Alert") Nothing 0) dialogBody), wStatus)
  where
    wStatus = hCenter $ txt "Alert message popped"
    dialogBody = drawLog alertMsg
drawWorkingArea NoWork
  = (Nothing, hCenter $ txt "Nothing to be done")
