module Battle.Menu where

import           Battle.Types
import           Control.Monad
import           Control.Monad.Except
import           Data.Foldable
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Void
import           Engine.Drawing
import           Engine.Types
import           Engine.Utils


menuButton :: BattleMenu -> Controls -> Bool
menuButton AttackMenu = c_ok
menuButton DefendMenu = c_cancel
menuButton ItemMenu   = c_item
menuButton SpellMenu  = c_spell


handleClose :: SF RawFrameInfo (Event BattleMenu)
handleClose = proc rfi -> do
  att   <- edge -< menuButton AttackMenu $ fi_controls rfi
  def   <- edge -< menuButton DefendMenu $ fi_controls rfi
  item  <- edge -< menuButton ItemMenu   $ fi_controls rfi
  spell <- edge -< menuButton SpellMenu  $ fi_controls rfi
  returnA -< asum
    [ AttackMenu <$ att
    , DefendMenu <$ def
    , ItemMenu   <$ item
    , SpellMenu  <$ spell
    ]

selectFighter
  :: (FighterId -> Bool)
  -> BattleMenu
  -> ExceptT BattleMenu (Swont r (RawFrameInfo, BattleState) Renderable) FighterId
selectFighter p me = ExceptT $ swont $ loopPre 0 $ proc ((rfi, bs), ix) -> do
  let parts = sortOn (view _x . bp_pos . snd) $ filter (p . fst) $ mapMaybe sequence $ M.assocs bs
      len = length $ parts
  let (who, sel_part) = parts !! ix

  close <- handleClose -< rfi
  let ev = do
        e <- close
        guard $ me == e
        pure $ Right who

  left   <- edge -< c_left  $ fi_controls rfi
  right  <- edge -< c_right $ fi_controls rfi

  let update_ix :: Event (Int -> Int)
      update_ix = fmap appEndo $ mconcat
        [ Endo (subtract 1) <$ left
        , Endo (+ 1)        <$ right
        ]

  returnA -<
    ( ( mconcat
          [ drawFilledRect (V4 255 0 255 255) $ Rectangle (P $ bp_pos sel_part - 10) 10
          , drawText 12 (V3 0 0 0) (display $ bp_name sel_part) $ bp_pos sel_part + V2 0 30
          ]
      , asum [ev, fmap Left close]
      )
    , clamp 0 (len - 1) $ fromEvent id update_ix ix
    )

menu
    :: (Display a)
    => [a]
    -> BattleMenu
    -> ExceptT BattleMenu (Swont r (RawFrameInfo, x) Renderable) a
menu opts me = ExceptT $ swont $ loopPre 0 $ proc ((rfi, _), ix) -> do
  close <- handleClose -< rfi
  let ev = do
        e <- close
        guard $ me == e
        pure $ Right $ opts !! ix

  up    <- edge -< c_up   $ fi_controls rfi
  down  <- edge -< c_down $ fi_controls rfi

  let update_ix :: Event (Int -> Int)
      update_ix = fmap appEndo $ mconcat
        [ Endo (clamp 0 (len - 1) . subtract 1) <$ up
        , Endo (clamp 0 (len - 1) . (+ 1))      <$ down
        ]

  let out :: Renderable
      out = mconcat
        [ drawFilledRect (V4 0 50 50 255) $ Rectangle (P $ V2 50 50) (V2 100 200)
        , mconcat $ do
           (opt, i) <- zip opts [0..]
           let y = 50 + fromIntegral i * 16
           pure $ mconcat
            [ ifA (i == ix) $
                drawFilledRect (V4 0 100 100 255) $ Rectangle (P $ V2 50 y) (V2 100 12)
            , drawText 12 (V3 255 255 255) (display opt) $ V2 50 y
            ]
        ]

  returnA -< ((out, asum [ ev, fmap Left close ] ), fromEvent id update_ix ix)
 where
  len = length opts


needsSelection :: BattleAction -> FighterSelection
needsSelection Attack = SelectEnemy
needsSelection Defend = NoSelection
needsSelection RunAway = NoSelection
needsSelection (UseItem Item_TestItem) = SelectHero
needsSelection (UseSpell Spell_TestSpell) = SelectEnemy

toPredicate :: FighterSelection -> FighterId -> Bool
toPredicate SelectEnemy EnemyKey = True
toPredicate SelectEnemy _ = False
toPredicate SelectHero (HeroKey _) = True
toPredicate SelectHero _ = False
toPredicate SelectAnyone _ = True
toPredicate NoSelection _ = False


battleMenu :: Maybe BattleMenu -> Swont r (RawFrameInfo, BattleState) Renderable (BattleAction, Maybe FighterId)
battleMenu m = do
  result <- runExceptT $ do
    (mode, action) <- case m of
      Nothing -> ExceptT $ swont $ arr fst >>> handleClose >>> arr ((mempty, ) . fmap Left)
      Just AttackMenu -> fmap (AttackMenu, ) $ menu [Attack] AttackMenu
      Just DefendMenu -> fmap (DefendMenu, ) $ menu [Defend, RunAway] DefendMenu
      Just ItemMenu   -> fmap ((ItemMenu, )  . UseItem)  $ menu [minBound @Item  .. maxBound] ItemMenu
      Just SpellMenu  -> fmap ((SpellMenu, ) . UseSpell) $ menu [minBound @Spell .. maxBound] SpellMenu
    who <- case needsSelection action of
      NoSelection -> pure Nothing
      sel         -> fmap Just $ selectFighter (toPredicate sel) mode
    pure (action, who)
  either (battleMenu . Just) pure result




menuObject :: FighterId -> ObjSF BattleMessage Void FighterId (Maybe BattleFighter)
menuObject owner =
  proc oi -> do
    sw <- getSwont $ battleMenu Nothing -< (oi_fi oi, oi_everyone oi)
    returnA -<
      case sw of
        Left o  ->
          ObjectOutput
            { oo_commands = mempty
            , oo_outbox = mempty
            , oo_state = Nothing
            , oo_render = o
            }
        Right r ->
          ObjectOutput
            { oo_commands = pure Unspawn
            , oo_outbox = [(owner, SomeMsg DoAction r)]
            , oo_state = Nothing
            , oo_render = mempty
            }

