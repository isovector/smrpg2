module Battle.Scripts.Jump where

import Data.Maybe
import qualified Data.Map as M
import Battle.Common
import Data.Function


jumpScript :: BattleState -> V2 Double -> FighterId -> BattleScript
jumpScript st pos target = do
  let tpos  = bp_pos $ fromJust $ st M.! target
      halfway = (pos + (tpos - pos) / 2)
  spriteLerp 0.25 pos halfway Mario_Battle_Idle
  spriteLerp 0.15 halfway (halfway - V2 0 jumpSize) Mario_Battle_JumpUp
  num_jumps <- dswont $ proc oi -> do
    ((off, anim), done) <- jump -< oi_fi oi
    oo <- drawMe -< (oi, anim, const $ tpos - off)
    returnA -< (oo, done)
  spriteLerp 0.3 tpos pos Mario_Battle_Idle
  pure $ mempty { ar_messages = pure (target, SomeMsg DoDamage $ num_jumps * 10) }


jumpSize :: Num a => a
jumpSize = 200

jumpUp :: Swont r a (V2 Double, Anim) ()
jumpUp = lerpSF 0.1 $ arr $ \(t, _) -> (V2 0 (jumpSize * t), Mario_Battle_JumpUp)

jumpDown :: Swont r a (V2 Double, Anim) ()
jumpDown = do
  timed 0.5 $ constant $ (V2 0 jumpSize, Mario_Battle_JumpUp)
  lerpSF 0.1 $ arr $ \(t, _) -> (V2 0 (jumpSize * (1 - t)), Mario_Battle_JumpDown)

timedJump :: SF RawFrameInfo ((V2 Double, Anim), Event TimedHitResult)
timedJump = proc i -> do
  (p, ev) <- keeping (0, Mario_Battle_JumpStomp) $ getSwont jumpDown -< i
  hit <- timedHit SpellMenu -< (i, ev)
  returnA -< (p, hit)

jump :: SF RawFrameInfo ((V2 Double, Anim), Event Int)
jump = keeping (0, Mario_Battle_JumpStomp) $ getSwont $ fix $ \loop -> do
  thr <- swont timedJump
  case (thr >= Good) of
    True  -> jumpUp >> fmap (+ 1) loop
    False -> pure 0

