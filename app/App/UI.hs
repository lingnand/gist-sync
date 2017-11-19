module App.UI
  (
    attrMap
  , drawUI
  ) where

import App.Types
import Brick
import Brick.Widgets.Border

attrMap :: AppState -> AttrMap
attrMap = _

drawUI :: AppState -> [Widget Name]
drawUI st = _

-- drawConfig :: AppConfig -> Bk.Widget n
-- drawConfig AppConfig{..} =
--   Bk.borderWithLabel (Bk.str ""
