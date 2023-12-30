{-# LANGUAGE AllowAmbiguousTypes                 #-}
{-# LANGUAGE FunctionalDependencies              #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Engine.Resources where

import           Control.Monad ((<=<))
import qualified Data.Map as M
import           Data.OctTree
import           Data.Semigroup
import           Data.Traversable (for)
import           Engine.Types
import           Engine.Utils (setGroundOrigin)
import           Numeric.Lens (hex)
import           SDL (Texture, textureWidth, textureHeight)
import           SDL.JuicyPixels (loadJuicyTexture)
import           SDL.Video (queryTexture)
import           System.Environment.Blank (getEnv)
import           System.FilePath
-- import qualified Sound.ALUT as ALUT


class (Ord key, Bounded key, Enum key)
      => IsResource key res
       | key -> res
       where
  resourceFolder :: String
  resourceExt :: String
  resourceName :: key -> String
  load :: key -> Engine -> FilePath -> IO res

-- instance {-# OVERLAPPABLE #-} (Enum a, Bounded a) => GEnum a where
--   genum = enumFromTo minBound maxBound

resourceRootPath :: IO FilePath
resourceRootPath =
  maybe "resources" (</> "usr/share/ld52-exe/resources") <$> getEnv "APPDIR"

loadResource
    :: forall key res
     . IsResource key res
    => FilePath -> Engine -> IO (key -> res)
loadResource rpath engine = do
  m <- fmap M.fromList $
    for [minBound .. maxBound] $ \k ->
      fmap (k, ) $ load @_ @res k engine $
        rpath </> resourceFolder @key @res </>
          resourceName k <.> resourceExt @key @res
  pure $ \k -> m M.! k

newtype Char' = Char' { getChar' :: Char }
  deriving (Eq, Ord, Show, Enum)

instance Bounded Char' where
  minBound = Char' $ toEnum 32
  maxBound = Char' $ toEnum 122


frameCounts :: Anim -> Int
frameCounts Mario_Battle_Idle = 1
frameCounts Mario_Battle_Defend = 1
frameCounts Mario_Battle_JumpUp = 1
frameCounts Mario_Battle_JumpDown = 1
frameCounts Mario_Battle_JumpStomp = 1

-- frameSound :: Anim -> Int -> Maybe Sound
-- frameSound (Run _) 1 = Just StepSound
-- frameSound (Run _) 3 = Just StepSound
-- frameSound _ _ = Nothing

wrapTexture :: Texture -> IO WrappedTexture
wrapTexture t = do
  q <- queryTexture t
  pure $ WrappedTexture
    { getTexture = t
    , wt_size = V2 (textureWidth q) $ textureHeight q
    , wt_sourceRect = Nothing
    , wt_origin = 0
    }

scaleWrapped :: Float -> WrappedTexture -> WrappedTexture
scaleWrapped sc = #wt_size %~ fmap (round . (* sc) . fromIntegral)

instance IsResource Anim [WrappedTexture] where
  resourceFolder = "sprites/"
  resourceExt = "png"
  resourceName _ = "unused"
  load an e _ = do
    rpath <- resourceRootPath
    for [0 .. frameCounts an - 1] $ \i -> do
      let fp = rpath </> "sprites/" </> animName an <> "_" <> show i <.> "png"
      wt <- wrapTexture =<< loadJuicyTexture (e_renderer e) fp
      pure $ setGroundOrigin $ scaleWrapped 1.5 wt


animName :: Anim -> FilePath
animName Mario_Battle_Idle = "mario/battle-idle"
animName Mario_Battle_Defend = "mario/battle-defend"
animName Mario_Battle_JumpUp = "mario/battle-jumpup"
animName Mario_Battle_JumpDown = "mario/battle-jumpdown"
animName Mario_Battle_JumpStomp = "mario/battle-jumpstomp"


-- charName :: Sprite -> FilePath
-- charName MainCharacter = "mc"


loadWrappedTexture :: Engine -> FilePath -> IO WrappedTexture
loadWrappedTexture
  = (wrapTexture <=<)
  . loadJuicyTexture
  . e_renderer

instance IsResource Char' Texture where
  load _
      = loadJuicyTexture
      . e_renderer
  resourceFolder = "font"
  resourceExt    = "png"
  resourceName c = "font-" <> pad 3 '0' (show $ fromEnum c)

pad :: Int -> Char -> String -> String
pad n c s =
  let len = length s
   in case len >= n of
        True -> s
        False -> replicate (n - len) c <> s

-- instance IsResource GameTexture WrappedTexture where
--   load _
--       = (wrapTexture <=<)
--       . loadJuicyTexture
--       . e_renderer
--   resourceFolder = "textures"
--   resourceExt    = "png"
--   resourceName NintendoLogo = "nintendo"
--   resourceName ChickenTexture = "chicken"
--   resourceName Parallax0 = "parallax0"
--   resourceName Parallax1 = "parallax1"
--   resourceName Parallax2 = "parallax2"
--   resourceName ChargeTexture = "charge"
--   resourceName TeleTexture = "teleball"
--   resourceName AuraTexture = "aura"
--   resourceName TrampolineTexture = "trampoline"
--   resourceName KeycapTexture = "key_unpressed"
--   resourceName CheckpointTexture = "checkpoint"
--   resourceName ActiveCheckpointTexture = "checkpoint-active"
--   resourceName EggTexture = "coin"
--   resourceName ArrowTexture = "green_arrow"

-- instance IsResource Song ALUT.Source where
--   load _ _ fileName = do
--     buf <- ALUT.createBuffer (ALUT.File fileName)
--     src <- ALUT.genObjectName
--     ALUT.loopingMode src ALUT.$= ALUT.Looping
--     ALUT.buffer src ALUT.$= Just buf
--     pure src
--   resourceFolder = "songs"
--   resourceExt    = "wav"
--   resourceName WarmDuckShuffle = "warm-duck-shuffle"

-- instance IsResource Sound ALUT.Source where
--   load _ _ fileName = do
--     buf <- ALUT.createBuffer (ALUT.File fileName)
--     src <- ALUT.genObjectName
--     ALUT.loopingMode src ALUT.$= ALUT.OneShot
--     ALUT.buffer src ALUT.$= Just buf
--     pure src
--   resourceFolder = "sounds"
--   resourceExt    = "wav"
--   resourceName NintendoSound = "ding"
--   resourceName CheckpointSound = "checkpoint"
--   resourceName CoinSound = "coin"
--   resourceName DieSound = "die"
--   resourceName JumpSound = "jump"
--   resourceName StepSound = "step"
--   resourceName ThudSound = "thud"
--   resourceName WarpSound = "warp"

instance IsResource World (OctTree (Maybe Color)) where
  load _ _ fp = do
    fc <- readFile fp
    pure
      $ fuse
      $ coerce @(OctTree (Maybe (Last Color)))
      $ foldr (uncurry $ flip insert) mempty $ do
          l <- filter ((/= "#") . take 1) $ lines fc
          let (x : y : z : (r1 : r2 : g1 : g2 : b1 : b2 : []) : []) = words l
              rs = r1 : r2 : []
              gs = g1 : g2 : []
              bs = b1 : b2 : []
          let Just (V3 r g b) = traverse (preview hex) $ V3 rs gs bs
              col = V4 r g b 255
          pure $ (Just $ Last $ col, fmap read $ V3 x y z)
  resourceFolder = "levels"
  resourceExt    = "txt"
  resourceName TestWorld = "test"


loadResources :: Engine -> IO Resources
loadResources engine = do
  rpath <- resourceRootPath

  -- textures <- loadResource rpath engine
  -- songs    <- loadResource rpath engine
  -- sounds   <- loadResource rpath engine
  worlds   <- loadResource rpath engine
  anims    <- loadResource rpath engine
  glyphs   <- loadResource rpath engine

  pure $ Resources
    { r_engine   = engine
    -- , r_textures = textures
    -- , r_sounds   = sounds
    -- , r_songs    = songs
    , r_worlds   = worlds
    , r_anims    = anims
    , r_glyphs   = glyphs . Char'
    }

