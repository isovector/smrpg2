{-# LANGUAGE CPP #-}

module Engine.Drawing where

import           Data.Foldable (for_, traverse_)
import           Engine.FRP
import           Engine.Geometry (rectContains)
import           Engine.Globals
import           Engine.Types
import           Engine.Utils (originRectToRect)
import           Foreign.C
import           SDL
import qualified Sound.ALUT as ALUT


drawOriginRect :: Color -> OriginRect Double -> V2 Double -> Renderable
drawOriginRect c ore = drawFilledRect c . originRectToRect (coerce ore)


drawFilledRect :: Color -> Rectangle Double -> Renderable
drawFilledRect c (Rectangle (P v) sz) = do
  let rect' = Rectangle (P v) $ coerce sz
  let renderer = e_renderer $ r_engine global_resources
  rendererDrawColor renderer $= c
  fillRect renderer $ Just $ fmap (round) rect'

drawBackgroundColor :: Color -> Renderable
drawBackgroundColor c = do
  let renderer = e_renderer $ r_engine global_resources
  rendererDrawColor renderer $= c
  fillRect renderer Nothing

drawSpriteStretched
    :: WrappedTexture  -- ^ Texture
    -> V2 Double       -- ^ position
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> V2 Double       -- ^ scaling factor
    -> Renderable
drawSpriteStretched wt pos theta flips stretched
  | let wp = pos - coerce (fmap fromIntegral (wt_origin wt) * stretched)
  -- , rectContains screenRect wp
  = do
      let renderer = e_renderer $ r_engine global_resources
      copyEx
        renderer
        (getTexture wt)
        (wt_sourceRect wt)
        (Just $ fmap round
              $ Rectangle (P $ coerce wp)
              $ fmap fromIntegral (wt_size wt) * stretched)
        (CDouble theta)
        (Just $ fmap round
              $ P
              $ fmap fromIntegral (wt_origin wt) * stretched)
        flips
  | otherwise = mempty

-- drawGameTextureOriginRect
--     :: GameTexture
--     -> OriginRect Double
--     -> V2 Double     -- ^ position
--     -> Double          -- ^ rotation in rads
--     -> V2 Bool         -- ^ mirroring
--     -> Renderable
-- drawGameTextureOriginRect = drawTextureOriginRect . global_textures

drawTextureOriginRect
    :: WrappedTexture  -- ^ Texture
    -> OriginRect Double
    -> V2 Double     -- ^ position
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> Renderable
drawTextureOriginRect wt ore pos theta flips
  | let wp = pos
  -- , rectContains screenRect wp
  = do
      let renderer = e_renderer $ r_engine global_resources
      copyEx
        renderer
        (getTexture wt)
        (wt_sourceRect wt)
        (Just $ fmap round $ originRectToRect ore $ coerce wp)
        (CDouble theta)
        (Just $ P $ fmap round $ orect_offset ore)
        flips
  | otherwise = mempty

drawSprite
    :: WrappedTexture
    -> V2 Double  -- ^ pos
    -> Double     -- ^ rotation in rads
    -> V2 Bool    -- ^ mirroring
    -> Renderable
drawSprite wt pos theta flips =
  drawSpriteStretched wt pos theta flips 1

-- mkAnim :: SF (DrawSpriteDetails, V2 Double) Renderable
-- mkAnim = proc (dsd, pos) -> do
--   let anim = dsd_anim dsd
--   global_tick <- round . (/ 0.1) <$> localTime -< ()
--   new_anim <- onChange -< dsd_anim dsd
--   anim_start <- hold 0 -< global_tick <$ new_anim

--   let anim_frame = (global_tick - anim_start) `mod` frameCounts anim
--   new_frame <- onChange -< anim_frame

--   returnA -< \cam -> do
--     for_ new_frame $ traverse_ playSound . frameSound anim
--     drawSprite
--       (global_anims anim !! anim_frame)
--       pos
--       (dsd_rotation dsd)
--       (dsd_flips dsd)
--       cam


atScreenPos :: Renderable -> Renderable
atScreenPos = id


drawText :: Double -> V3 Word8 -> String -> V2 Double -> Renderable
drawText sz color text pos@(V2 x y)
  -- | rectContains screenRect pos
  = do
      let renderer = e_renderer $ r_engine global_resources
      for_ (zip text [0..]) $ \(c, i) -> do
        let glyph = global_glyphs c
        textureColorMod glyph $= color
        copy renderer glyph Nothing
          $ Just
          $ fmap round
          $ Rectangle (P $ coerce $ V2 (x + coerce (i * sz)) $ withDescender sz c y)
          $ V2 sz sz
      rendererDrawBlendMode renderer $= BlendAlphaBlend
  -- | otherwise = mempty

withDescender :: Double -> Char -> Double -> Double
withDescender sz 'j' = (+ coerce sz / 6)
withDescender sz 'g' = (+ coerce sz / 5)
withDescender sz 'y' = (+ coerce sz / 6)
withDescender sz 'p' = (+ coerce sz / 6)
withDescender sz 'q' = (+ coerce sz / 6)
withDescender _  _   = id


