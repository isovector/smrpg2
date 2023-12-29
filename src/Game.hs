{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Game where

import Data.Ord
import           Battle.Common
import           Battle.Menu
import           Battle.Scripts.Jump
import           Control.Lens (_Just)
import           Data.Foldable
import           Data.List
import qualified Data.Map as M
import           Data.Maybe (fromJust, mapMaybe, listToMaybe)
import           Data.Typeable


horton :: BattleFighter
horton = BattleFighter "Horton" 100 HeroTeam (V4 255 0 0 255) 300


skalp :: BattleFighter
skalp = BattleFighter "Skalp" 100 HeroTeam (V4 255 255 255 255) 400


baddie :: BattleFighter
baddie = BattleFighter "Baddie" 20 EnemyTeam (V4 0 0 0 255) $ V2 500 200


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
  cc <- battleRouter (Ephemeral
                        . maybe 0 (+ 1)
                        . listToMaybe
                        . sortOn Down
                        . mapMaybe (preview #_Ephemeral)
                        . M.keys
                     ) (const $ \case)
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
            pos = bp_pos $ fromJust $ oi_state oi
        handle_dmg <- damageHandler -< oi
        returnA -< handle_dmg $ ObjectOutput
          { oo_render =
              case isAlive oi of
                True -> drawFilledRect col
                      $ Rectangle (P pos) 10
                False -> drawText 15 (V3 128 0 128) "X" pos
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


damageIndicatorTime :: Time
damageIndicatorTime = 0.8

damageIndicator :: V2 Double -> Int -> SF OI OO
damageIndicator pos0 dmg = let dur = damageIndicatorTime in proc _ -> do
  t   <- localTime -< ()
  die <- after dur () -< ()
  let pos = pos0 - (V2 0 50) * pure (min 1 (2 * t / dur))
  let dmg_s = show dmg

  returnA -< ObjectOutput
    { oo_render   = mconcat
        [ drawFilledRect (V4 0 0 0 128) (Rectangle (P pos)
            $ V2 (fromIntegral $ length dmg_s * 10) 10)
        , drawText 10 (V3 255 0 0) dmg_s pos
        ]
    , oo_state    = Nothing
    , oo_outbox   = mempty
    , oo_commands = [ Unspawn
                    | Event _ <- pure die
                    ]
    }

damageHandler :: SF OI (OO -> OO)
damageHandler = proc oi -> do
  let mailbox :: Typeable v => BattleMessage v -> [(KEY, v)]
      mailbox = oie_mailbox $ oi_inbox oi

      raw_dmg = fmap snd $ mailbox DoDamage
      has_dmg = not $ null raw_dmg
      dmg = sum raw_dmg
      pos = bp_pos $ fromJust $ oi_state oi

  recv_dmg <- delayEvent damageIndicatorTime -< bool NoEvent (Event dmg) has_dmg

  returnA -< (#oo_state . _Just . #bp_hp -~ event 0 id recv_dmg)
           . (#oo_commands <>~ [ Spawn Nothing Nothing $ damageIndicator pos dmg
                               | has_dmg
                               ])

