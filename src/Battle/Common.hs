module Battle.Common
  ( module Battle.Common
  , module Battle.TimedHits
  , module Battle.Types
  , module Engine.Drawing
  , module Engine.Types
  , module Game.FRP
  ) where

import Battle.TimedHits
import Battle.Types
import Engine.Drawing
import Engine.Types
import Game.FRP


drawMe :: SF (OI, Anim, V2 Double -> V2 Double) OO
drawMe = proc (oi, anim, offset) -> do
  out <- drawSimpleSprite -< (offset (bp_pos $ oi_state' oi), anim)
  returnA -< hoistOO out oi


