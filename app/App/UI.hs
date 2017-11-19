module App.UI
  (
    getAttrMap
  , drawUI
  ) where

import App.Types
import Brick
import Brick.Widgets.Border

getAttrMap :: AppState -> AttrMap
getAttrMap = _

drawUI :: AppState -> [Widget Name]
drawUI st = _

-- drawConfig :: AppConfig -> Bk.Widget n
-- drawConfig AppConfig{..} =
--   borderWithLabel (str "Config") $
--     center ("")
