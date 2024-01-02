{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

module Engine.Drawing
  ( module Engine.Drawing
  , Cube (..)
  ) where

import           Control.Monad
import           Data.Foldable (for_)
import           Data.OctTree (Cube(..), cubeCorners)
import           Data.OctTree.Internal (pattern Oct8)
import qualified Data.Vector.Storable as V
import           Engine.FRP
import           Engine.Globals
import           Engine.Resources
import           Engine.Types
import           Engine.Utils (originRectToRect)
import           Foreign.C
import           SDL
import qualified SDL.Raw.Types as Raw
-- import qualified Sound.ALUT as ALUT


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

mkAnim :: SF (DrawSpriteDetails, V2 Double) Renderable
mkAnim = proc (dsd, pos) -> do
  let anim = dsd_anim dsd
  global_tick <- round . (/ 0.1) <$> localTime -< ()
  new_anim <- onChange -< dsd_anim dsd
  anim_start <- hold 0 -< global_tick <$ new_anim

  let anim_frame = (global_tick - anim_start) `mod` frameCounts anim
  -- new_frame <- onChange -< anim_frame

  returnA -< do
    -- for_ new_frame $ traverse_ playSound . frameSound anim
    drawSprite
      (global_anims anim !! anim_frame)
      pos
      (dsd_rotation dsd)
      (dsd_flips dsd)



atScreenPos :: Renderable -> Renderable
atScreenPos = id


drawText :: Double -> V3 Word8 -> String -> V2 Double -> Renderable
drawText sz color text _pos@(V2 x y)
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

tileWidth, tileHeight, tileUp :: Num a => a
tileWidth = 32
tileHeight = 18
tileUp = 32

toIsoSpace :: V3 Rational -> V2 Double
toIsoSpace (fmap fromRational -> V3 x y z)
  = V2 (500 + (tileWidth * x + tileWidth * y) / 2) (500 + (tileHeight * x - tileHeight * y - tileUp * z) / 2)

toIsoSpace' :: V3 Rational -> Raw.FPoint
toIsoSpace' (toIsoSpace -> V2 x y)
  = Raw.FPoint (fromRational $ toRational x) (fromRational $ toRational  y)

drawVoxel :: Cube Rational -> Color -> Renderable
drawVoxel (cubeCorners -> Oct8 tl0 tr0 bl0 br0 tl1 tr1 bl1 br1) (V4 r g b a) = do
  let renderer = e_renderer $ r_engine global_resources
      col :: Double -> Raw.Color
      col x   = Raw.Color (round $ fromIntegral r * x)
                          (round $ fromIntegral g * x)
                          (round $ fromIntegral b * x) a
      tcol    = col 1
      lcol    = col 0.9
      rcol    = col 0.8
      uv       = Raw.FPoint 0 0
      vertices c = V.fromList
                   [ Vertex (toIsoSpace' tl1) c uv
                   , Vertex (toIsoSpace' tr1) c uv
                   , Vertex (toIsoSpace' bl1) c uv
                   , Vertex (toIsoSpace' br1) c uv
                   , Vertex (toIsoSpace' tl0) c uv
                   , Vertex (toIsoSpace' tr0) c uv
                   , Vertex (toIsoSpace' bl0) c uv
                   , Vertex (toIsoSpace' br0) c uv
                   ]
  renderGeometry renderer Nothing
       (vertices tcol)
       (V.fromList [
                     0, 2, 3 -- top face
                   , 0, 1, 3
                   ])
  renderGeometry renderer Nothing
       (vertices lcol)
       (V.fromList [
                     4, 5, 1 -- left face
                   , 0, 1, 4
                   ])
  renderGeometry renderer Nothing
       (vertices rcol)
       (V.fromList [
                     1, 3, 5 -- right face
                   , 3, 5, 7
                   ])
  rendererDrawColor renderer $= V4 0 0 0 64
  when (a == 0) $ do
    drawLines renderer $ V.fromList $ fmap (P. fmap round)
      [ toIsoSpace tl1
      , toIsoSpace tr1
      , toIsoSpace br1
      , toIsoSpace bl1
      , toIsoSpace tl1
      ]
    drawLines renderer $ V.fromList $ fmap (P. fmap round)
      [ toIsoSpace tl1
      , toIsoSpace tr1
      , toIsoSpace tr0
      , toIsoSpace tl0
      , toIsoSpace tl1
      ]
    drawLines renderer $ V.fromList $ fmap (P. fmap round)
      [ toIsoSpace tr0
      , toIsoSpace br0
      , toIsoSpace br1
      , toIsoSpace tr1
      , toIsoSpace tr0
      ]

drawDebugVoxel :: Cube Rational -> Renderable
drawDebugVoxel (cubeCorners -> Oct8 tl0 tr0 bl0 br0 tl1 tr1 bl1 br1) = do
  let renderer = e_renderer $ r_engine global_resources
  let ps = [ (toIsoSpace tl1, V4 255 0 0 255)
           , (toIsoSpace tr1, V4 255 255 0 255)
           , (toIsoSpace bl1, V4 0 255 0 255)
           , (toIsoSpace br1, V4 0 255 255 255)
           , (toIsoSpace tl0, V4 0 0 255 255)
           , (toIsoSpace tr0, V4 255 0 255 255)
           , (toIsoSpace bl0, V4 255 255 255 255)
           , (toIsoSpace br0, V4 0 0 0 255)
           ]
  for_ ps $ \(pos, col) -> do
    rendererDrawColor renderer $= col
    drawPoint renderer $ P $ fmap round pos

draw3dPoint :: V3 Rational -> Color -> Renderable
draw3dPoint p col = do
  let renderer = e_renderer $ r_engine global_resources
  rendererDrawColor renderer $= col
  drawPoint renderer $ P $ fmap round $ toIsoSpace p

withDescender :: Double -> Char -> Double -> Double
withDescender sz 'j' = (+ coerce sz / 6)
withDescender sz 'g' = (+ coerce sz / 5)
withDescender sz 'y' = (+ coerce sz / 6)
withDescender sz 'p' = (+ coerce sz / 6)
withDescender sz 'q' = (+ coerce sz / 6)
withDescender _  _   = id


