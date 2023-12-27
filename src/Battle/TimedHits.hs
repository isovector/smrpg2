module Battle.TimedHits where

import Battle.Menu
import Battle.Types
import Control.Applicative
import Engine.Types


timedHit :: BattleMenu -> SF (RawFrameInfo, Event a) (Event TimedHitResult)
timedHit bm = loopPre (-9999, noEvent, noEvent) $ proc ((rfi, raw_ev), (last_attempt, last_ok, seen)) -> do
  let cooldown = 0.3
      grace    = 0.12
      flub     = 0.2
      perfect  = 0.05
  t <- time -< ()
  let ev = t <$ raw_ev
  attempt_press <- edge -< menuButton bm $ fi_controls rfi
  let is_attempt = isEvent attempt_press
  let attempt_ev = t <$ attempt_press
  let ok_ev =
        case is_attempt && t - last_attempt >= cooldown of
          True  -> attempt_ev
          False -> noEvent
  returnA
    -< ( case (seen, last_ok) of
           (Event real, Event hit)
             | abs (real - hit) <= perfect -> Event Perfect
             | abs (real - hit) <= grace   -> Event Good
             | otherwise                   -> Event Flubbed
           (Event real, _)
             | t >= real + flub          -> Event Unattempted
           (_, _)                          -> noEvent
       , (fromEvent last_attempt attempt_ev, ok_ev <|> last_ok, ev <|> seen))

