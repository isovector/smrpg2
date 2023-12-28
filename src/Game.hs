{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Game where

import Data.Maybe
import           Battle.Menu
import           Battle.TimedHits
import           Battle.Types
import           Control.Lens (at, _Just)
import           Control.Monad.Except
import           Data.Foldable
import qualified Data.Map as M
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


heroHandler :: ObjSF BattleMessage Void FighterId (Maybe BattleFighter)
heroHandler = foreverSwont $ do
  (action, target) <- swont $ proc oi -> do
    me <- drawMe -< (oi, id)
    let ev = asum $ fmap (Event . snd) $ oie_mailbox (oi_inbox oi) DoAction
    returnA -< (me, ev)
  case action of
    UseSpell Spell_TestSpell -> do
      dswont $ proc oi -> do
        (off, done) <- jump -< oi_fi oi
        oo <- drawMe -< (oi, subtract off)
        returnA -< ( oo
                        & #oo_commands <>~ [ Spawn (Just Menu) Nothing $ menuObject $ HeroKey Hero1 ]
                   , done)
    Defend -> do
      let dur = 2
      dswont $ proc oi -> do
        end <- after dur () -< ()
        oo <- drawMe -< (oi, subtract 20)
        returnA -< ( oo
                        & #oo_commands <>~ [ Spawn (Just Menu) Nothing $ menuObject $ HeroKey Hero1 ]
                   , end)
  where
    drawMe = proc (oi, offset) -> do
      returnA -< ObjectOutput
        { oo_state = oi_state oi
        , oo_commands = mempty
        , oo_outbox = mempty
        , oo_render =
            mconcat
              [ drawFilledRect (V4 0 0 0 128)
                $ Rectangle (P $ bp_pos $ oi_state' oi) 10
              , drawFilledRect (V4 128 0 0 255)
                $ Rectangle (P $ offset (bp_pos $ oi_state' oi)) 10
              ]
        }


game :: SF RawFrameInfo Renderable
game = testRouter

jumpSize :: Num a => a
jumpSize = 300

jumpUp :: Swont r a (V2 Double) ()
jumpUp = lerpSF 0.1 $ arr $ \t -> V2 0 (jumpSize * t)

jumpDown :: Swont r a (V2 Double) ()
jumpDown = do
  timed 0.5 $ constant $ V2 0 jumpSize
  lerpSF 0.1 $ arr $ \t -> V2 0 (jumpSize * (1 - t))

timedJump :: SF RawFrameInfo (V2 Double, Event TimedHitResult)
timedJump = proc i -> do
  (p, ev) <- keeping 0 $ getSwont jumpDown -< i
  hit <- timedHit SpellMenu -< (i, ev)
  returnA -< (p, hit)

jump :: SF RawFrameInfo (V2 Double, Event ())
jump = keeping 0 $ getSwont $ fix $ \loop -> do
  jumpUp
  thr <- swont timedJump
  case (thr >= Good) of
    True  -> loop
    False -> pure ()



testRouter :: SF RawFrameInfo Renderable
testRouter = proc rfi -> do
  cc <- router @BattleMessage @Void @FighterId @(Maybe BattleFighter) undefined (\case) $ ObjectMap mempty $ M.fromList
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

