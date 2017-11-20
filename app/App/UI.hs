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
import           Data.Foldable (toList)
import           Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.Format
import qualified SyncState as SS

textPad :: Int
textPad = 3

getAttrMap :: AppState -> AttrMap
getAttrMap = _

drawUI :: AppState -> [Widget Name]
drawUI st = _

drawKV :: Show a => String -> a -> Widget n
drawKV k v = padAll textPad $ str (k++": "++show v)

drawConfig :: AppConfig -> Widget n
drawConfig AppConfig{..} =
  borderWithLabel (str "Config") $
    drawKV "sync-state-storage" syncStateStorage
    <=>
    (drawKV "sync-dir" syncDir
     <+>
     drawKV "sync-interval" syncInterval)

drawActionHistory
  :: Int
  -> Seq.Seq (Timestamped SS.SyncAction')
  -> Widget n
drawActionHistory = drawSequence "Recent actions" shower
  where shower act = T.pack $ show act

drawLogs
  :: Int
  -> Seq.Seq (Timestamped LogMsg)
  -> Widget n
drawLogs = drawSequence "Log" shower
  where shower (LogMsg lvl msg) = T.pack (show lvl) <> msg

drawSequence
  :: T.Text        -- ^ title
  -> (a -> T.Text) -- ^ function to show
  -> Int           -- ^ number of most recent items to show
  -> Seq.Seq (Timestamped a)
  -> Widget n
drawSequence title shower nRecent seq =
  borderWithLabel (txt title) $
    padAll textPad . center . txtWrap . T.unlines . toList $ shower' <$> items
  where items = Seq.drop (max 0 (Seq.length seq - nRecent)) seq
        shower' (t, a) = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t)
                      <> " " <> shower a
