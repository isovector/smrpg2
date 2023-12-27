{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Game where

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
baddie = BattleFighter "Baddie" 100 EnemyTeam (V4 0 0 0 255) $ V2 400 200


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
  (action, _target) <- swont $ proc oi -> do
    me <- drawMe 0 -< oi
    let ev = asum $ fmap (Event . snd) $ oie_mailbox (oi_inbox oi) DoAction
    returnA -< (me, ev)
  case action of
    Defend -> do
      let dur = 2
      dswont $ proc oi -> do
        end <- after dur () -< ()
        oo <- drawMe (-20) -< oi
        returnA -< ( oo
                        & #oo_commands <>~ [Spawn (Just Menu) undefined $ menuObject $ HeroKey Hero1 ]
                   , end)
      pure ()
  where
    drawMe :: V2 Double -> ObjSF BattleMessage Void FighterId (Maybe BattleFighter)
    drawMe offset = proc oi -> do
      returnA -< ObjectOutput
        { oo_state = oi_state oi
        , oo_commands = mempty
        , oo_outbox = mempty
        , oo_render = drawFilledRect (V4 0 0 0 255)
            $ Rectangle (P $ (bp_pos $ oi_state' oi) + offset) 10
        }


game :: SF RawFrameInfo Renderable
game = testRouter


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
    , (Menu, (Nothing , menuObject $ HeroKey Hero1))
    ] -< rfi
  returnA -< foldMap oo_render cc

