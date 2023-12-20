module Engine.Controls where

import           Engine.Types
import qualified SDL.Input as SDL
import           SDL.Input.Keyboard.Codes


parseControls :: (SDL.Scancode -> Bool) -> Controls
parseControls check = Controls
  { c_left  = check ScancodeLeft
  , c_right = check ScancodeRight
  , c_up    = check ScancodeUp
  , c_down  = check ScancodeDown
  , c_ok  = check ScancodeZ
  , c_cancel  = check ScancodeX
  , c_dir   =
      V2
        (toOne ScancodeRight - toOne ScancodeLeft)
        (toOne ScancodeDown - toOne ScancodeUp)
  }
  where
    toOne code = if check code then 1 else 0

