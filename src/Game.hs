{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Game where

import           Battle.Menu
import           Battle.TimedHits
import           Battle.Types
import           Control.Monad.Except
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Void
import           Engine.Drawing
import           Engine.Router
import           Engine.Types


horton :: BattleFighter
horton = BattleFighter "Horton" 100 HeroTeam (V4 255 0 0 255) 300


skalp :: BattleFighter
skalp = BattleFighter "Skalp" 100 HeroTeam (V4 255 255 255 255) 400


baddie :: BattleFighter
baddie = BattleFighter "Baddie" 100 EnemyTeam (V4 0 0 0 255) $ V2 500 200


testTimedHits :: SF RawFrameInfo Renderable
testTimedHits = runSwont undefined $ fix $ \loop -> do
  timed 2 $ proc rfi -> do
    ev <- after 1 () -< ()
    thr' <- timedHit DefendMenu -< (rfi, ev)
    thr <- once -< thr'
    res <- hold Nothing -< fmap Just thr
    returnA -<
      case (ev, res) of
        (Event _, _) -> drawBackgroundColor $ V4 0 0 0 255
        (_, Nothing) -> mempty
        (_, Just Perfect) -> drawBackgroundColor $ V4 0 255 0 255
        (_, Just Good) -> drawBackgroundColor $ V4 255 255 0 255
        (_, Just Flubbed) -> drawBackgroundColor $ V4 255 0 0 255
        (_, Just Unattempted) -> drawBackgroundColor $ V4 255 0 255 255
  loop


lerpBetween
    :: Ord k
    => Time
    -> V2 Double
    -> V2 Double
    -> Anim
    -> Swont r (ObjectInput msg k STATE)
               (ObjectOutput msg c k STATE)
               ()
lerpBetween dur start end anim =
  lerpSF dur $ proc (t, oi) -> do
    oo <- actuallyDraw -< (start * (pure $ 1 - t) + end * pure t, anim)
    returnA -< hoistOO oo oi

actuallyDraw :: SF (V2 Double, Anim) Renderable
actuallyDraw = proc (pos, anim) -> do
  mkAnim -< (DrawSpriteDetails anim 0 $ pure False, pos)

hoistOO :: Ord k => Renderable -> ObjectInput msg k STATE -> ObjectOutput msg c k STATE
hoistOO out oi =
  ObjectOutput
    { oo_state = oi_state oi
    , oo_commands = mempty
    , oo_outbox = mempty
    , oo_render =
        mconcat
          [ drawFilledRect (V4 0 0 0 128)
            $ Rectangle (P $ bp_pos $ oi_state' oi) 10
          , out
          ]
    }

drawMe :: SF (OI, Anim, V2 Double -> V2 Double) OO
drawMe = proc (oi, anim, offset) -> do
  out <- actuallyDraw -< (offset (bp_pos $ oi_state' oi), anim)
  returnA -< hoistOO out oi

heroHandler :: SF OI OO
heroHandler = foreverSwont $ do
  ((action, target0), oi0) <- swont $ proc oi -> do
    me <- drawMe -< (oi, Mario_Battle_Idle, id)
    let ev = asum $ fmap (Event . snd) $ oie_mailbox (oi_inbox oi) DoAction
    returnA -< (me, fmap (, oi) ev)
  AttackResult msgs cmds <- case action of
    UseSpell Spell_TestSpell -> do
      let target = fromJust target0
      let tpos  = bp_pos $ fromJust $ oi_everyone oi0 M.! target
          pos = bp_pos $ fromJust $ oi_state oi0
          halfway = (pos + (tpos - pos) / 2)
      lerpBetween 0.25 pos halfway Mario_Battle_Idle
      lerpBetween 0.15 halfway (halfway - V2 0 jumpSize) Mario_Battle_JumpUp
      num_jumps <- dswont $ proc oi -> do
        ((off, anim), done) <- jump -< oi_fi oi
        oo <- drawMe -< (oi, anim, const $ tpos - off)
        returnA -< (oo, done)
      lerpBetween 0.3 tpos pos Mario_Battle_Idle
      pure $ mempty { ar_messages = pure (target, SomeMsg DoDamage $ num_jumps * 10) }
    Defend -> do
      let dur = 2
      dswont $ proc oi -> do
        end <- after dur mempty -< ()
        oo <- drawMe -< (oi, Mario_Battle_Defend, id)
        returnA -< (oo, end)
  dswont $ proc oi -> do
    end <- after 0.0016 () -< ()
    oo <- drawMe -< (oi, Mario_Battle_Idle, id)
    returnA -< ( oo
                    & #oo_outbox   <>~ msgs
                    & #oo_commands <>~ (Spawn (Just Menu) Nothing $ menuObject $ HeroKey Hero1)
                                     : cmds
                , end)


game :: SF RawFrameInfo Renderable
game = testRouter

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



testRouter :: SF RawFrameInfo Renderable
testRouter = proc rfi -> do
  cc <- router @BattleMessage
               @Void
               @FighterId
               @(Maybe BattleFighter)
          undefined
          (const $ \case)
      $ ObjectMap mempty $ M.fromList
    [
      (HeroKey Hero1, (Just horton, heroHandler))
    , (HeroKey Hero2, (Just skalp, proc oi -> do
        let col = V4 0 0 0 255
        returnA -< ObjectOutput
          { oo_render = drawFilledRect col
            $ Rectangle (P $ 400) 10
          , oo_state = oi_state oi
          , oo_outbox = mempty
          , oo_commands = mempty
          }
      ))
    , (EnemyKey, (Just baddie, proc oi -> do
        let col = V4 128 0 128 255
        returnA -< ObjectOutput
          { oo_render = drawFilledRect col
            $ Rectangle (P $ V2 500 200) 10
          , oo_state = oi_state oi
          , oo_outbox = mempty
          , oo_commands = mempty
          }
      ))
    , (Menu, (Nothing , menuObject $ HeroKey Hero1))
    ] -< rfi
  returnA -< mconcat
    [ foldMap oo_render cc
    ]

