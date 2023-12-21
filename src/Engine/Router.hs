{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}

module Engine.Router where

import           Control.Lens (at, non)
import           Control.Monad
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable
import           Engine.Types hiding (tag)

spawn
    :: Ord k
    => (forall x. ObjectMap msg k x -> k)
    -> Maybe k
    -> ObjSF msg k s
    -> ObjectMap msg k (ObjSF msg k s)
    -> ObjectMap msg k (ObjSF msg k s)
spawn gen Nothing sf m = m & #objm_map %~ M.insert (gen m) sf
spawn _ (Just k) sf m = m & #objm_map %~ M.insert k sf

send
    :: (Show k, Ord k)
    => k
    -> k
    -> SomeMsg msg
    -> ObjectMap msg k (ObjSF msg k s)
    -> ObjectMap msg k (ObjSF msg k s)
send from to msg = #objm_undeliveredMsgs . at to . non mempty <>~ [(traceShowId from, msg)]

recv :: forall k v msg. (Typeable v, Eq (msg v)) => [(k, SomeMsg msg)] -> msg v -> [(k, v)]
recv [] _ = []
recv ((from, SomeMsg key (val :: v')) : xs) tag
  | Just Refl <- eqT @v @v'
  , key == tag
  = (from, val) : recv xs tag
  | otherwise = recv xs tag

router
    :: forall msg s k
     . ( Show k, Ord k
       , forall v. Eq (msg v)
       )
    => (forall x. ObjectMap msg k x -> k)
    -> ObjectMap msg k (ObjSF msg k s)
    -> SF RawFrameInfo (ObjectMap msg k (ObjectOutput msg k s))
router gen st =
  loopPre mempty $
    router' gen st <&> \om ->
      (om, fmap oo_state $ objm_map om)

router'
    :: forall msg k s
     . ( Show k, Ord k
       , forall v. Eq (msg v)
       )
    => (forall x. ObjectMap msg k x -> k)
    -> ObjectMap msg k (ObjSF msg k s)
    -> SF (RawFrameInfo, Map k s) (ObjectMap msg k (ObjectOutput msg k s))
router' gen st =
  pSwitch
    @(ObjectMap msg k)
    @(RawFrameInfo, Map k s)
    @(ObjectInput msg k s)
    @(ObjectOutput msg k s)
    @(Endo (ObjectMap msg k (ObjSF msg k s)))
    (\(rfi, prev) col -> col & #objm_map %~
        (M.mapWithKey $ \k ->
          (ObjectInput rfi k prev
            $ ObjectInEvents
            $ recv
            $ join
            $ maybeToList
            $ M.lookup k
            $ objm_undeliveredMsgs col
          , )))
    st
    ((arr (\(_, om) ->
      flip mappend (pure $ Endo id)
        $ flip foldMap (M.toList $ objm_map om)
        $ \(k, oo_events -> ObjectEvents {..}) ->
            mconcat
              [ Endo (#objm_map %~ M.delete k)       <$  oe_die
              , foldMap (Endo . uncurry (spawn gen)) <$> oe_spawn
              , foldMap (Endo . uncurry (send k))    <$> oe_send_message
              ])
     >>> notYet)
    )
    (\new f -> router' gen $ appEndo f $ new & #objm_undeliveredMsgs .~ mempty)

