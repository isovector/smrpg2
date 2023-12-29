module Game.FRP where

import Engine.Drawing
import Engine.Types


drawSimpleSprite :: SF (V2 Double, Anim) Renderable
drawSimpleSprite = proc (pos, anim) -> do
  mkAnim -< (DrawSpriteDetails anim 0 $ pure False, pos)


spriteLerp
    :: Ord k
    => Time
    -> V2 Double
    -> V2 Double
    -> Anim
    -> Swont r (ObjectInput msg k s)
               (ObjectOutput msg c k s)
               ()
spriteLerp dur start end anim =
  lerpSF dur $ proc (t, oi) -> do
    oo <- drawSimpleSprite -< (start * (pure $ 1 - t) + end * pure t, anim)
    returnA -< hoistOO oo oi


hoistOO :: Ord k => Renderable -> ObjectInput msg k s -> ObjectOutput msg c k s
hoistOO out oi =
  ObjectOutput
    { oo_state = oi_state oi
    , oo_commands = mempty
    , oo_outbox = mempty
    , oo_render = out
    }

