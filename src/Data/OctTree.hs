module Data.OctTree
  ( -- * Important types
    OctTree
  , Region (..)
  , V3 (..)

    -- * Construction
  , makeTree
  , insert
  , fill

    -- * Destruction
  , foldTree
  , tightlySatisfying
  , hitTest
  , hitTestR
  , pointMap
  , getLocation
  , asWeighted
  , volumize

  -- * Modifying
  , renormalize
  , tighten
  , cookieCut

    -- * Helpers
  , bounds
  , inBounds

    -- * Subdivision
  , Oct (..)
  , subdivide
  , corners

    -- * Geometry
  , containsRegion
  , containsPoint
  , intersects
  , getIntersection
  , regionSize
  , regionPoints
  ) where

import           Control.Arrow (first)
import           Data.Bool (bool)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.OctTree.Geometry
import           Data.OctTree.Internal (Octant, Soctant)
import qualified Data.OctTree.Internal as I
import           GHC.Generics (Generic)
import           Linear.V3
import Data.Maybe (mapMaybe)


data OctTree a = OctTree
  { qt_quad :: Octant a
  , qt_size :: Region
  }
  deriving (Show, Read, Eq, Functor, Generic, Traversable)
  deriving (Semigroup, Monoid) via Ap OctTree a

instance Applicative OctTree where
  pure a = OctTree (pure a) mempty
  OctTree fq fr <*> OctTree aq ar = OctTree (fq <*> aq) (fr <> ar)

instance Foldable OctTree where
  foldMap = foldTree . const


foldTree :: Monoid m => (Region -> a -> m) -> OctTree a -> m
foldTree f = I.foldTree f . regionify


fill :: Monoid a => a -> Region -> OctTree a -> OctTree a
fill a r = liftTree $ I.fill a r


insert :: Monoid a => a -> V3 Int -> OctTree a -> OctTree a
insert a r = liftTree $ I.insert a r


getLocation :: Monoid a => OctTree a -> V3 Int -> a
getLocation q v =  I.getLocation v $ regionify q


regionify :: OctTree a -> Soctant a
regionify (OctTree q r) = (r, q)


asWeighted :: OctTree a -> [a]
asWeighted = (uncurry replicate . first I.regionSize  =<<) . volumize


makeTree :: Monoid a => Region -> OctTree a
makeTree r = OctTree (I.Leaf mempty) r


liftTree :: (Soctant a -> Octant a) -> OctTree a -> OctTree a
liftTree f w = w { qt_quad = f $ regionify w }


volumize :: OctTree a -> [(Region, a)]
volumize = I.volumize . regionify


pointMap :: OctTree a -> Map (V3 Int) a
pointMap
  = foldTree (\r a -> M.fromList $ fmap (, a) $ regionPoints r)


hitTest :: Monoid m => (a -> m) -> Region -> OctTree a -> m
hitTest f r = I.hitTest (const f) r . regionify


hitTestR :: Monoid m => (Region -> a -> m) -> Region -> OctTree a -> m
hitTestR f r = I.hitTest f r . regionify


bounds :: OctTree a -> Region
bounds = qt_size


inBounds :: OctTree a -> Region -> Bool
inBounds = containsRegion . bounds


tightlySatisfying :: (a -> Bool) -> OctTree a -> Region
tightlySatisfying f =
  foldTree $ \r a -> bool mempty r $ f a


------------------------------------------------------------------------------
-- | Map the space contained by the OctTree.
--
-- $O(1)$
renormalize :: (Region -> Region) -> OctTree a -> OctTree a
renormalize f (OctTree q r) = OctTree q $ f r


------------------------------------------------------------------------------
-- | Change the bounds of the OctTree to be a bounding box satisfying the
-- predicate.
--
-- $O(n)$
tighten :: Monoid a => (a -> Bool) -> OctTree a -> OctTree a
tighten f q = cookieCut (tightlySatisfying f q) q


------------------------------------------------------------------------------
-- | Cut out the given region of the OctTree.
--
-- $O(n)$
cookieCut :: Monoid a => Region -> OctTree a -> OctTree a
cookieCut r q = do
  let pm = volumize q
      keep = mapMaybe (\(r', a) -> fmap (, a) $ getIntersection r r') pm
      q' = makeTree r
  foldr (uncurry $ flip fill) q' keep

