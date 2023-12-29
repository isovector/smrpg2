module Battle.Damage where

import Battle.Common
import Data.Maybe


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
  let raw_dmg = fmap snd $ inbox oi DoDamage
      has_dmg = not $ null raw_dmg
      dmg = sum raw_dmg
      pos = bp_pos $ fromJust $ oi_state oi

  recv_dmg <- delayEvent damageIndicatorTime -< bool NoEvent (Event dmg) has_dmg

  returnA -< (#oo_state . #_Just . #bp_hp -~ event 0 id recv_dmg)
           . (#oo_commands <>~ [ Spawn Nothing Nothing $ damageIndicator pos dmg
                               | has_dmg
                               ])

