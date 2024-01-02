{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}

module Game where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe, isJust)
import           Data.OctTree
import           Data.OctTree.Internal (pattern Oct8)
import           Data.Ratio
import           Data.Semigroup (Any(..))
import           Engine.Drawing
import           Engine.Globals
import           Engine.Types
import           Linear.Metric (normalize)
import           Linear.V3 (_z)

type Key a = (a, a)

sortKey :: Num a => Cube a -> Key a
sortKey (cubeCorners -> Oct8 (V3 x y z) _ _  _  _ _ _ _ ) = (x - y, z)


geometry :: Map (Key Rational) Renderable
geometry
  = M.fromListWith (<>)
  $ fmap (second $ (\(r, c) -> flip drawVoxel c r))
  $ fmap ((sortKey . fst) &&& id)
  -- $ ((Cube (V3 (-16) (-16) 0) (V3 32 32 1), V4 32 16 0 64) :)
  $ mapMaybe sequence
  $ toCubes
  $ global_worlds TestWorld


onPress :: (Controls -> Bool) -> a -> SF RawFrameInfo (Event a)
onPress sel a = arr (sel . fi_controls) >>> edge >>> arr (a <$)


renderScene :: Map a Renderable -> Renderable
renderScene = mconcat . M.elems


arrows :: RawFrameInfo -> V2 Double
arrows (fi_controls -> c) = normalize $ sum
  [ bool 0 (V2 (-1) 0) $ c_left c
  , bool 0 (V2 1 0)    $ c_right c
  , bool 0 (V2 0 (-1)) $ c_down c
  , bool 0 (V2 0 1)    $ c_up c
  ]

player :: SF RawFrameInfo (Cube Rational)
player = let sz = V3 (9 % 10) (9 % 10) (16 % 10) in loopPre (Cube @Rational (V3 0 0 1) sz) $ proc (rfi, Cube pos _) -> do
  let V2 x y = arrows rfi
      wanted_pos = pos + (fmap toRational $ (V3 x y 0) ^* (5 * fi_deltaTime rfi))
      wanted_cube = Cube wanted_pos sz
      can_move = not $ getAny $ query (Any . isJust) wanted_cube $ global_worlds TestWorld

  returnA -< dup $ Cube (bool pos wanted_pos can_move) sz


game :: SF RawFrameInfo Renderable
game = proc rfi -> do
  c@(Cube pos sz) <- player -< rfi
  let center = pos + ((sz / 2) & _z .~ 0)
  mario <- mkAnim -< (DrawSpriteDetails Mario_Battle_Idle 0 $ pure False, toIsoSpace center + V2 0 5)

  returnA -< mconcat
    [ renderScene $ M.insertWith (<>) (sortKey c) (mconcat
        [ mario
        , drawVoxel c $ V4 0 255 0 64
        ]) geometry
    ]

