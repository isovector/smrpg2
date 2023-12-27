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
  , module Data.Generics.Product
  , (*^)
  , (^*)
  , (<&>)
  , _x
  , _y
  , distance
  , toList
  , bool
  ) where

import           Control.Lens ((&), (^.), (.~), (%~), (+~), (-~), (<>~), view, set, over, preview, review, (<&>))
import           Data.Bool (bool)
import           Data.Coerce
import           Data.Foldable (toList)
import           Data.Generics.Labels ()
import           Data.Generics.Product (field, field')
import           Data.Kind
import qualified Data.Map as M
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Typeable
import           Data.Word
import           Debug.Trace (trace, traceShowId, traceM)
import           Engine.FRP hiding (left, right, loop)
import           Foreign.C (CInt)
import           GHC.Generics
import           SDL hiding (trace, Event, Display)
-- import qualified Sound.ALUT as ALUT

data Controls = Controls
  { c_up     :: Bool
  , c_down   :: Bool
  , c_left   :: Bool
  , c_right  :: Bool
  , c_dir    :: V2 Int
  , c_ok     :: Bool
  , c_cancel :: Bool
  , c_spell  :: Bool
  , c_item   :: Bool
  }
  deriving (Eq, Ord, Show, Read)


type SomeMsg :: (Type -> Type) -> Type
data SomeMsg msg where
  SomeMsg :: Typeable t => msg t -> t -> SomeMsg msg

instance Eq (SomeMsg msg) where
  _ == _ = False




type ObjectMap :: (Type -> Type) -> Type -> Type -> Type -> Type
data ObjectMap msg k s a = ObjectMap
  { objm_undeliveredMsgs :: Map k [(k, SomeMsg msg)]
  , objm_map :: Map k (s, a)
  }
  deriving stock (Functor, Generic, Foldable)

data Message a where
  deriving stock (Eq, Ord, Show, Read, Generic)

type ObjSF msg c k s = SF (ObjectInput msg k s) (ObjectOutput msg c k s)

data Command msg c k s
  = Die
  | Spawn (Maybe k) ~s (ObjSF msg c k s)
  | Broadcast (SomeMsg msg)
  | OtherCommand c
  deriving stock (Generic)

type ObjectInEvents :: (Type -> Type) -> Type -> Type
data ObjectInEvents msg k = ObjectInEvents
  { oie_mailbox :: forall v. Typeable v => msg v -> [(k, v)]
  }

instance Show (ObjectInEvents msg k) where
  show _ = "<InEvents>"

instance Semigroup (ObjectInEvents msg k) where
  ObjectInEvents a1 <> ObjectInEvents b1 =
    ObjectInEvents (a1 <> b1)

instance Monoid (ObjectInEvents msg k) where
  mempty = ObjectInEvents mempty

type ObjectInput :: (Type -> Type) -> Type -> Type -> Type
data ObjectInput msg k s = ObjectInput
  { oi_fi       :: RawFrameInfo
  , oi_self     :: k
  , oi_everyone :: Map k s
  , oi_inbox    :: ObjectInEvents msg k
  }
  deriving stock (Functor, Generic, Show)

oi_state :: Ord k => ObjectInput msg k s -> s
oi_state oi = fromMaybe (error "uh oh; no state!") $ M.lookup (oi_self oi) $ oi_everyone oi

type ObjectOutput :: (Type -> Type) -> Type -> Type -> Type -> Type
data ObjectOutput msg c k s = ObjectOutput
  { oo_outbox   :: [(k, SomeMsg msg)]
  , oo_commands :: [Command msg c k s]
  , oo_render   :: Renderable
  , oo_state    :: ~s
  }
  deriving stock (Generic)

instance Semigroup s => Semigroup (ObjectOutput msg c k s) where
  ObjectOutput a1 a2 a3 a4 <> ObjectOutput b1 b2 b3 b4 =
    ObjectOutput
      (a1 <> b1)
      (a2 <> b2)
      (a3 <> b3)
      (a4 <> b4)





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
  , fi_ext :: a
  }
  deriving stock (Show, Generic)

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
