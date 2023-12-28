{-# OPTIONS_GHC -Wno-orphans #-}

module Engine.FRP
  ( module Engine.FRP
  , module FRP.Yampa
  ) where

import Control.Monad.Cont
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Monoid
import FRP.Yampa hiding ((*^), fromEvent)

newtype Swont r i o a = Swont
  { runSwont' :: ContT (o, Event r) (SF i) a
  }
  deriving newtype (Functor, Applicative, Monad)

swont :: SF a (b, Event c) -> Swont r a b c
swont = Swont . ContT . switch . (>>> arr (\(b, ev) -> ((b, NoEvent), ev)))

dswont :: SF a (b, Event c) -> Swont r a b c
dswont = Swont . ContT . switch . (>>> arr (\(b, ev) -> ((b, NoEvent), ev)))

getSwont :: Swont r i o r -> SF i (Either o r)
getSwont (runSwont' -> sw) = proc i -> do
  (o, ev) <- (runContT sw $ \r -> pure (undefined, Event r)) -< i
  returnA -< event (Left o) Right ev

runSwont :: (a -> SF i o) -> Swont r i o a -> SF i o
runSwont k (runSwont' -> sw) = fmap fst $ runContT sw $ fmap (, NoEvent) . k

runSwontForever :: Swont r i o r -> SF i (o, Event r)
runSwontForever sw@(Swont c)
  = runContT c
  $ \r -> (second (const $ Event r) --> arr (const id)) <*> runSwontForever sw


waitFor :: SF a (Event c) -> SF a b -> Swont r a b c
waitFor ev sf = dswont $ (,) <$> sf <*> ev

waitForEdge :: (a -> Bool) -> SF a b -> Swont r a b ()
waitForEdge f = waitFor (arr f >>> edge)


timed :: Double -> SF a b -> Swont r a b ()
timed dur sf = waitFor (after dur ()) sf


lerpSF :: Double -> SF Double b -> Swont r a b ()
lerpSF dur sf = timed dur $ localTime >>> arr (/ dur) >>> sf

keeping :: o -> SF i (Either o e) -> SF i (o, Event e)
keeping o0 sf = proc i -> do
  x <- sf -< i
  e <- once -< either (const NoEvent) Event x
  o <- hold o0 -< either Event (const NoEvent) x
  returnA -< (o, e)


timedSequence
    :: SF i o    -- ^ final result
    -> Double    -- ^ duration of each sf
    -> [SF i o]  -- ^ sfs to run
    -> SF i o
timedSequence d interval sfs =
  runSwont (const d) $
    traverse_ (swont . (&&& after interval ())) sfs


foreverSwont :: Swont r i o a -> SF i o
foreverSwont = runSwont (error "impossible") . forever


deriving via (Ap (SF i) o) instance Semigroup o => Semigroup (SF i o)
deriving via (Ap (SF i) o) instance Monoid o    => Monoid    (SF i o)

deriving stock instance Foldable Event
deriving stock instance Traversable Event

instance Semigroup o => Semigroup (Event o) where
  (<>) = mergeBy (<>)

instance Semigroup o => Monoid (Event o) where
  mempty = noEvent


get :: (i -> a) -> SF i o -> Swont r i o a
get f sf = dswont $ proc i -> do
  o <- sf -< i
  returnA -< (o, Event $ f i)


-- -- | Perform the given action for a single frame, rendering the next step of
-- -- the Swont for that frame.
-- momentary :: Semigroup o => o -> Swont r i o ()
-- momentary what = Swont $ cont $ \ f ->
--   dSwitch
--     (proc i -> do
--       io <- constant what -< ()
--       k  <- f () -< i
--       ev <- now () -< ()
--       returnA -< (io <> k, ev)
--     )
--     $ const $ f ()

-- data Resumption s o = Resumption
--   { r_state  :: !s
--   , r_output :: !o
--   , r_stop  :: !(Event ())
--   }
--   deriving stock Functor

-- instance Bifunctor Resumption where
--   bimap fab fcd (Resumption a c ev) = Resumption
--     { r_state = fab a
--     , r_output = fcd c
--     , r_stop = ev
--     }

-- -- | A 'Resumable' is a signal function with state. The final state is returned
-- -- by 'runResumable', meaning you can resume it exactly where you left off.
-- newtype Resumable s i o = Resumable
--   { unResumable :: SF (s, i) (Resumption s o)
--   }
--   deriving stock Functor

-- runResumable :: s -> Resumable s i o -> Swont r i o s
-- runResumable s0 (Resumable sf) = swont $ loopPre s0 $
--   proc is -> do
--     Resumption s' o ev <- sf -< swap is
--     returnA -< ((o, s' <$ ev), s')

fork :: [SF i o] -> SF i [o]
fork = par $ \i -> fmap (i, )

inject :: (a -> a) -> SF a b -> SF a b
inject f sf =
  dSwitch
    (proc a -> do
      b <- sf -< f a
      returnA -< (b, Event ())
    )
    (const sf)

eventToMaybe :: Event a -> Maybe a
eventToMaybe NoEvent = Nothing
eventToMaybe (Event a) = Just a

onChange :: Eq a => SF a (Event a)
onChange = proc a ->
  edgeBy (\old new -> bool Nothing new $ old /= new) Nothing -< Just a

fromEvent :: a -> Event a -> a
fromEvent a NoEvent = a
fromEvent _ (Event a') = a'

whenE :: Bool -> Event a -> Event a
whenE False _ = noEvent
whenE True ev = ev

