{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Game where

import           Battle.Common
import           Battle.Menu
import           Battle.Scripts.Jump
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe (fromJust)


horton :: BattleFighter
horton = BattleFighter "Horton" 100 HeroTeam (V4 255 0 0 255) 300


skalp :: BattleFighter
skalp = BattleFighter "Skalp" 100 HeroTeam (V4 255 255 255 255) 400


baddie :: BattleFighter
baddie = BattleFighter "Baddie" 100 EnemyTeam (V4 0 0 0 255) $ V2 500 200


heroHandler :: SF OI OO
heroHandler = foreverSwont $ do
  ((action, target0), oi0) <- swont $ proc oi -> do
    me <- drawMe -< (oi, Mario_Battle_Idle, id)
    let ev = asum $ fmap (Event . snd) $ oie_mailbox (oi_inbox oi) DoAction
    returnA -< (me, fmap (, oi) ev)
  AttackResult msgs cmds <- case action of
    UseSpell Spell_TestSpell ->
      jumpScript
        (oi_everyone oi0)
        (bp_pos $ fromJust $ oi_state oi0)
        (fromJust target0)
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


testRouter :: SF RawFrameInfo Renderable
testRouter = proc rfi -> do
  cc <- battleRouter undefined (const $ \case)
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

