module Data.OctTree.Geometry where

import Control.Applicative (liftA2)
import Data.Maybe (isJust)
import Data.Monoid
import GHC.Generics (Generic)
import Linear.V3

data Oct a
  = Oct !a !a
        !a !a

        !a !a
        !a !a
  deriving stock (Show, Read, Eq, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via (Ap Oct a)

instance Applicative Oct where
  pure a = Oct a a a a a a a a
  Oct a1 a2 a3 a4 a5 a6 a7 a8 <*> Oct  b1 b2 b3 b4 b5 b6 b7 b8 =
    Oct (a1 b1) (a2 b2) (a3 b3) (a4 b4) (a5 b5) (a6 b6) (a7 b7) (a8 b8)


data Region = Region
  { r_x :: !Int
  , r_y :: !Int
  , r_z :: !Int
  , r_w :: !Int
  , r_h :: !Int
  , r_d :: !Int
  }
  deriving stock (Show, Read, Eq, Generic, Ord)

-- | Gives a region which fully contains both
instance Semigroup Region where
  r1 <> r2
    | isEmptyRegion r1 = r2
    | isEmptyRegion r2 = r1
    | otherwise =
        let Oct tl@(V3 x y z) _ _ _ _ _ _ br =
              fmap liftA2 (Oct min const const const const const const max)
                <*> corners r1
                <*> corners r2
            V3 w h d = br - tl
        in Region x y z w h d

instance Monoid Region where
  mempty = Region 0 0 0 0 0 0


subdivide :: Region -> Oct Region
subdivide (Region x y z 1 1 1) =
  let r = Region x y z 0 0 0
   in Oct r r r r r r r r
subdivide (Region x y z w h d) =
  let halfw = div w 2
      halfh = div h 2
      halfd = div d 2
   in Oct
        (sanitizeRegion $ Region x y z halfw halfh halfd)
        (sanitizeRegion $ Region (x + halfw) y z (w - halfw) halfh halfd)
        (sanitizeRegion $ Region x (y + halfh) z halfw (h - halfh) halfd)
        (sanitizeRegion $ Region (x + halfw) (y + halfh) z (w - halfw) (h - halfh) halfd)
        (sanitizeRegion $ Region x y (z + halfd) halfw halfh (z - halfd))
        (sanitizeRegion $ Region (x + halfw) y (z + halfd) (w - halfw) halfh (z - halfd))
        (sanitizeRegion $ Region x (y + halfh) (z + halfd) halfw (h - halfh) (z - halfd))
        (sanitizeRegion $ Region (x + halfw) (y + halfh) (z + halfd) (w - halfw) (h - halfh) (z - halfd))


sanitizeRegion :: Region -> Region
sanitizeRegion r@(Region x y z w h d)
  | w <= 0 || h <= 0 || d <= 0
  = Region x y z 0 0 0
  | otherwise
  = r


containsRegion :: Region -> Region -> Bool
containsRegion r1@(Region bx by bz bw bh bd) r2@(Region sx sy sz sw sh sd) =
  r1 == r2 ||
  and
    [ bx <= sx
    , by <= sy
    , bz <= sz
    , sx + sw <= bx + bw
    , sy + sh <= by + bh
    , sz + sd <= bz + bd
    ]


containsPoint :: Region -> V3 Int -> Bool
containsPoint (Region _ _ _ w h d) _
  | w <= 0 || h <= 0 || d <= 0
  = False
containsPoint (Region x y z w h d) (V3 tx ty tz) =
  and
    [ x <= tx
    , y <= ty
    , z <= tz
    , tx < x + w
    , ty < y + h
    , tz < z + d
    ]


corners :: Region -> Oct (V3 Int)
corners (Region x y z w h d) =
  let p = V3 x y z
      dx = V3 w 0 0
      dy = V3 0 h 0
      dz = V3 0 0 d
   in fmap (p +) $ Oct 0   dx        dy       (dx + dy)
                       dz (dx + dz) (dy + dz) (dx + dy + dz)


intersects :: Region -> Region -> Bool
intersects r1 r2 = isJust $ getIntersection r1 r2


getIntersection :: Region -> Region -> Maybe Region
getIntersection r1 r2 =
  let x0 = max (r_x r1) (r_x r2)
      y0 = max (r_y r1) (r_y r2)
      z0 = max (r_z r1) (r_z r2)
      x1 = min (r_x r1 + r_w r1) (r_x r2 + r_w r2)
      y1 = min (r_y r1 + r_h r1) (r_y r2 + r_h r2)
      z1 = min (r_z r1 + r_d r1) (r_z r2 + r_d r2)
      w = x1 - x0
      h = y1 - y0
      d = z1 - z0
   in case 0 < w && 0 < h && 0 < d of
        True -> Just $ Region x0 y0 z0 w h d
        False -> Nothing


regionSize :: Region -> Int
regionSize (Region _ _ _ w h d) = w * h * d


regionPoints :: Region -> [V3 Int]
regionPoints (Region x y z w h d) = do
  zp <- [z .. z + d - 1]
  yp <- [y .. y + h - 1]
  xp <- [x .. x + w - 1]
  pure $ V3 xp yp zp

isEmptyRegion :: Region -> Bool
isEmptyRegion (Region _ _ _ w h d) = w <= 0 || h <= 0 || d <= 0

