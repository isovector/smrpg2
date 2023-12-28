{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Engine.Main where

import           Control.Monad
import           Data.IORef
import           Data.Time.Clock.System
import           Engine.Globals (veryUnsafeEngineIORef)
import           Engine.Types
import           Engine.Controls (parseControls)
-- import           Game.Splash (runIntro)
import           SDL hiding (Vector, copy, Stereo)
-- import qualified Sound.ALUT as ALUT
import           System.Exit
import Game


screenSize :: RealFloat a => V2 a
screenSize = V2 (h * aspectRatio) h
  where
    h = 540

aspectRatio :: RealFloat a => a
aspectRatio = 16 / 9

logicalSize :: RealFloat a => V2 a
logicalSize = V2 (h * aspectRatio) h
  where
    h = 540



main :: IO ()
main = do -- ALUT.withProgNameAndArgs ALUT.runALUT $ \_ _ -> do
  initializeAll

  window <- createWindow "Where's My Chicken, Man?" $ defaultWindow
    { windowInitialSize = fmap (round @Double) screenSize
    , windowGraphicsContext = OpenGLContext defaultOpenGL
    }
  ctx <- glCreateContext window
  glMakeCurrent window ctx
  renderer <- createRenderer window (-1) defaultRenderer
    { rendererType = AcceleratedRenderer
    , rendererTargetTexture = True
    }
  rendererScale renderer $= screenSize / logicalSize
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  cursorVisible $= False

  let engine = Engine
        { e_renderer = renderer
        , e_window = window
        }
  !() <- writeIORef veryUnsafeEngineIORef engine
  -- !rs <- pure global_resources

  tS <- getSystemTime
  let seconds = floatSeconds tS
  tRef <- newIORef seconds

  reactimate
    (pure $ FrameInfo (Controls False False False False 0 False False False False) ())
    (input window tRef)
    (output engine)
    game
    -- game
    -- runIntro
  quit


input :: Window -> IORef Double -> Bool -> IO (Double, Maybe RawFrameInfo)
input win tRef _ = do
  pumpEvents
  es <- pollEvents
  when (any (isQuit . eventPayload) es) $ do
    destroyWindow win
    exitSuccess
  seconds <- readIORef tRef
  tS <- getSystemTime
  let seconds' = floatSeconds tS
  writeIORef tRef seconds'

  let dt = seconds' - seconds

  keys <- getKeyboardState

  pure (dt, Just $ FrameInfo (parseControls keys) ())


pattern Keypress :: Scancode -> EventPayload
pattern Keypress scan <- KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym scan _ _))


isQuit :: EventPayload -> Bool
isQuit QuitEvent                   = True
isQuit (WindowClosedEvent _)       = True
isQuit (Keypress ScancodeEscape)   = True
isQuit (Keypress ScancodeCapsLock) = True
isQuit _                           = False


output :: Engine -> Bool -> (Renderable) -> IO Bool
output e _ (render) = do
  let renderer = e_renderer e
  rendererDrawColor renderer $= V4 100 149 237 255
  clear renderer
  render
  present renderer
  pure False


floatSeconds :: SystemTime -> Double
floatSeconds t
  = fromIntegral (systemSeconds t)
  + fromIntegral (systemNanoseconds t) / 1e9

