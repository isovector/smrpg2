module Data.OctTree.Internal
  ( module Data.OctTree.Internal
  , Region (..)
  , Oct (..)
  , regionSize
  ) where

import Data.Foldable (fold)
import Data.Monoid
import Data.OctTree.Geometry
import Linear.V3
import Control.Applicative (liftA2)
import GHC.Generics (Generic)
import Data.Bool (bool)


type Soctant a = (Region, Octant a)


data Octant a
  = Leaf a
  | Node (Oct (Octant a))
  deriving stock (Show, Read, Eq, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Ap Octant a

instance Applicative Octant where
  pure = Leaf
  Leaf f <*> a = fmap (f $) a
  f <*> Leaf a = fmap ($ a) f
  Node f <*> Node a = Node $ liftA2 (<*>) f a


-- TODO(sandy): doesn't satisfy the laws. not sure why, but I don't need this.

-- instance Monad Octant where
--   Leaf a >>= f = f a
--   Node (Oct tl tr bl br) >>= f =
--     Node $ Oct (tl >>= f) (tr >>= f) (bl >>= f) (br >>= f)


insert :: Monoid a => a -> V3 Int -> Soctant a -> Octant a
insert v (V3 x y z) = fill v (Region x y z 1 1 1)


hitTest
    :: forall m a
     . Monoid m
    => (Region -> a -> m)
    -> Region
    -> Soctant a
    -> m
hitTest _ what _
  | isEmptyRegion what = mempty
hitTest _ _ (r, Leaf _)
  | isEmptyRegion r = mempty
hitTest f what (r, Leaf a)
  | intersects what r
  = f r a
  | otherwise
  = mempty
hitTest f what (r, Node q) =
  fold $ origami (const mempty) (hitTest f) what (r, q)


foldTree
    :: forall m a
     . Monoid m
    => (Region -> a -> m)
    -> Soctant a
    -> m
foldTree _ (r, Leaf _)
  | isEmptyRegion r = mempty
foldTree f (r, Leaf a)
  = f r a
foldTree f (r, Node q) =
  foldMap (foldTree f) $
    (,) <$> subdivide r <*> q


origami
    :: (a -> b)                      -- ^ What to do if there is no intersection
    -> (Region -> (Region, a) -> b)  -- ^ What to do on an intersection
    -> Region                        -- ^ Looking for what
    -> (Region, Oct a)              -- ^ In the unnested quad
    -> Oct b
origami miss hit what (r, q) =
  let subr = subdivide r
      -- if we want to hit the whole region, we can skip the intersection
      -- checks
      subw = fmap (bool (getIntersection what) (Just) (r == what)) subr
      -- sel :: Maybe Region -> Region -> Octant a -> m
      sel Nothing _ q'   = miss q'
      sel (Just w) r' q' = hit w (r',  q')
   in sel <$> subw <*> subr <*> q


splitLeaf :: a -> Oct (Octant a)
splitLeaf a = let l = Leaf a in Oct l l l l l l l l


fill :: Monoid a => a -> Region -> Soctant a -> Octant a
fill _ what q | isEmptyRegion what = snd q
fill _ _ q@(r, Leaf{})
  | isEmptyRegion r
  = snd q
fill v what q@(r, Leaf a)
  | containsRegion what r
  = Leaf (v <> a)
  | intersects what r
  = subFill v what (r, splitLeaf a)
  | otherwise
  = snd q
fill v what (r, Node qu) = subFill v what (r, qu)


volumize :: Soctant a -> [(Region, a)]
volumize q = foldTree ((pure .) . (,)) q


subFill :: Monoid a => a -> Region -> (Region, Oct (Octant a)) -> Octant a
subFill v what q = Node $ origami id (fill v) what q


getLocation :: Monoid a => V3 Int -> Soctant a -> a
getLocation (V3 x y z) = hitTest (const id) (Region x y z 1 1 1)

