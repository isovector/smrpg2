{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Engine.Types
  ( module Engine.Types
  , V2 (..)
  , V3 (..)
  , V4 (..)
  , Rectangle (..)
  , Point (..)
  , Generic
  , Word8
  , module Debug.Trace
  , module Engine.FRP
  , SF
  , Event
  , coerce
  , module Control.Lens
  , (*^)
  , (^*)
  , _x
  , _y
  , distance
  , toList
  , bool
  ) where

import           Control.Lens ((&), (^.), (.~), (%~), (+~), (-~), (<>~), view, set, over, preview, review)
import           Data.Bool (bool)
import           Data.Coerce
import           Data.Foldable (toList)
import           Data.Generics.Labels ()
import           Data.Word
import           Debug.Trace (trace, traceShowId, traceM)
import           Foreign.C (CInt)
import           Engine.FRP
import           GHC.Generics
import           SDL hiding (trace, Event, Display)
-- import qualified Sound.ALUT as ALUT

data Controls = Controls
  { c_up    :: Bool
  , c_down  :: Bool
  , c_left  :: Bool
  , c_right :: Bool
  , c_dir   :: V2 Int
  , c_ok      :: Bool
  , c_cancel  :: Bool
  }
  deriving (Eq, Ord, Show, Read)

data GameState = GameState
  deriving (Eq, Ord, Show, Read)

data Anim = Anim
  deriving (Eq, Ord, Show, Read)



------------------------------------------------------------------------------

data Engine = Engine
  { e_renderer :: Renderer
  , e_window :: Window
  }

------------------------------------------------------------------------------
-- | Things we need to keep track of, like sprites and music and stuff.
data Resources = Resources
  { r_engine   :: Engine
  -- , r_textures :: GameTexture -> WrappedTexture
  -- , r_sounds   :: Sound -> ALUT.Source
  -- , r_songs    :: Song -> ALUT.Source
  , r_anims    :: Anim -> [WrappedTexture]
  , r_glyphs   :: Char -> Texture
  }


------------------------------------------------------------------------------

type Color = V4 Word8

type Renderable = IO ()


------------------------------------------------------------------------------
-- | Things that change every frame.
data FrameInfo' a = FrameInfo
  { fi_controls :: Controls
  , fi_dt :: Double
  , fi_global :: ~a
  }
  deriving stock Generic

type FrameInfo = FrameInfo' ()
type RawFrameInfo = FrameInfo' ()

data WrappedTexture = WrappedTexture
  { getTexture    :: Texture
  , wt_sourceRect :: Maybe (Rectangle CInt)
  , wt_size       :: V2 CInt
  , wt_origin     :: V2 CInt
  }
  deriving stock Generic


data OriginRect aff = OriginRect
  { orect_size   :: V2 aff
  , orect_offset :: V2 aff
  }
  deriving (Eq, Ord, Show, Functor, Generic)

data Camera = Camera

-- instance Semigroup Camera where
--   (Camera v2) <> (Camera v2') = Camera $ v2 + v2'

-- WHY DOESNT THIS EXIST
instance (Bounded b, Enum a, Enum b) => Enum (a, b) where
  toEnum n =
    let a = n `div` (1 + fromEnum (maxBound @b))
        b = n `mod` (1 + fromEnum (maxBound @b))
     in (toEnum a, toEnum b)
  fromEnum (a, b) = fromEnum a * (1 + fromEnum (maxBound @b)) + fromEnum b


data DrawSpriteDetails = DrawSpriteDetails
  { dsd_anim :: Anim
  , dsd_rotation :: Double
  , dsd_flips :: V2 Bool
  }
  deriving stock (Eq, Ord, Show, Read, Generic)


------------------------------------------------------------------------------
  --
class HasFrameInfo a where
  frameInfo :: a -> FrameInfo

-- instance HasFrameInfo ObjectInput where
--   frameInfo = oi_frameInfo

class HasDeltaTime a where
  deltaTime :: a -> Time

instance HasDeltaTime (FrameInfo' a) where
  deltaTime = fi_dt

instance {-# OVERLAPPABLE #-} HasFrameInfo a => HasDeltaTime a where
  deltaTime = deltaTime . frameInfo

class HasControls a where
  controls :: a -> Controls

instance HasControls (FrameInfo' a) where
  controls = fi_controls

instance {-# OVERLAPPABLE #-} HasFrameInfo a => HasControls a where
  controls = controls . frameInfo


class Display a where
  display :: a -> String

instance Display String where
  display = id
