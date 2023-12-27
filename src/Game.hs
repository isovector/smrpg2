{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Game where

import Control.Lens (at, _Just)
import qualified Data.Map as M
import Data.Map (Map)
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Foldable
import Data.List
import Data.Monoid
import Engine.Drawing
import Engine.Router
import Engine.Types
import Engine.Utils
import Data.Void


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

type BattleState = Map FighterId BattleFighter

selectFighter
  :: (FighterId -> Bool)
  -> BattleMenu
  -> ExceptT BattleMenu (Swont r (RawFrameInfo, BattleState) Renderable) FighterId
selectFighter p me = ExceptT $ swont $ loopPre 0 $ proc ((rfi, bs), ix) -> do
  let parts = sortOn (view _x . bp_pos . snd) $ filter (p . fst) $ M.assocs bs
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
menu opts me = ExceptT $ swont $ loopPre 0 $ proc ((rfi, x), ix) -> do
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


data BattleMenu = AttackMenu | DefendMenu | ItemMenu | SpellMenu
  deriving (Eq, Ord, Show, Enum, Bounded)

data Item = Item_TestItem
  deriving (Eq, Ord, Show, Enum, Bounded)

data Team = HeroTeam | EnemyTeam | NoTeam
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Display Item where
  display = drop 5 . show

instance Display Spell where
  display = drop 6 . show

data Spell = Spell_TestSpell
  deriving (Eq, Ord, Show, Enum, Bounded)

data BattleAction
  = Attack
  | Defend
  | RunAway
  | UseItem Item
  | UseSpell Spell
  deriving (Eq, Ord, Show)

needsSelection :: BattleAction -> FighterSelection
needsSelection Attack = SelectEnemy
needsSelection Defend = NoSelection
needsSelection RunAway = NoSelection
needsSelection (UseItem Item_TestItem) = SelectHero
needsSelection (UseSpell Spell_TestSpell) = SelectEnemy

toPredicate :: FighterSelection -> FighterId -> Bool
toPredicate SelectEnemy _ = False
toPredicate SelectHero (HeroKey _) = True
toPredicate SelectHero _ = False
toPredicate SelectAnyone _ = True
toPredicate NoSelection _ = False

instance Display BattleAction where
  display = show

instance Display BattleMenu where
  display = show

data BattleFighter = BattleFighter
  { bp_name  :: String
  , bp_hp    :: Int
  , bp_team  :: Team
  , bp_color :: Color
  , bp_pos   :: V2 Double
  }
  deriving (Eq, Ord, Show, Generic)

instance Semigroup BattleFighter where
  BattleFighter a1 a2 a3 a4 a5 <> BattleFighter b1 b2 b3 b4 b5
    = BattleFighter (a1 <> b1) (a2 + b2) (a3) (a4 + b4) (a5 + b5)


horton :: BattleFighter
horton = BattleFighter "Horton" 100 HeroTeam (V4 255 0 0 255) 300


skalp :: BattleFighter
skalp = BattleFighter "Skalp" 100 HeroTeam (V4 255 255 255 255) 400


baddie :: BattleFighter
baddie = BattleFighter "Baddie" 100 EnemyTeam (V4 0 0 0 255) $ V2 400 200

menuFighter :: BattleFighter
menuFighter = BattleFighter "menu???" 100 NoTeam (V4 0 0 0 255) $ V2 400 200


data FighterSelection
  = SelectHero
  | SelectEnemy
  | SelectAnyone
  | NoSelection
  deriving (Eq, Ord, Show, Enum, Bounded)


data TimedHitResult = Unattempted | Flubbed | Good | Perfect
  deriving (Eq, Ord, Show, Enum, Bounded)

timedHit :: BattleMenu -> SF (RawFrameInfo, Event a) (Event TimedHitResult)
timedHit bm = loopPre (-9999, noEvent, noEvent) $ proc ((rfi, raw_ev), (last_attempt, last_ok, seen)) -> do
  let cooldown = 0.3
      grace    = 0.12
      flub     = 0.2
      perfect  = 0.05
  t <- time -< ()
  let ev = t <$ raw_ev
  attempt_press <- edge -< menuButton bm $ fi_controls rfi
  let is_attempt = isEvent attempt_press
  let attempt_ev = t <$ attempt_press
  let ok_ev =
        case is_attempt && t - last_attempt >= cooldown of
          True  -> attempt_ev
          False -> noEvent
  returnA
    -< ( case (seen, last_ok) of
           (Event real, Event hit)
             | abs (real - hit) <= perfect -> Event Perfect
             | abs (real - hit) <= grace   -> Event Good
             | otherwise                   -> Event Flubbed
           (Event real, _)
             | t >= real + flub          -> Event Unattempted
           (_, _)                          -> noEvent
       , (fromEvent last_attempt attempt_ev, ok_ev <|> last_ok, ev <|> seen))


testTimedHits :: SF RawFrameInfo Renderable
testTimedHits = runSwont undefined $ fix $ \loop -> do
  timed 2 $ proc rfi -> do
    ev <- after 1 () -< ()
    thr' <- timedHit DefendMenu -< (rfi, ev)
    thr <- once -< thr'
    res <- hold Nothing -< fmap Just thr
    returnA -<
      case (ev, res) of
        (Event _, _) -> drawBackgroundColor $ V4 0 0 0 255
        (_, Nothing) -> mempty
        (_, Just Perfect) -> drawBackgroundColor $ V4 0 255 0 255
        (_, Just Good) -> drawBackgroundColor $ V4 255 255 0 255
        (_, Just Flubbed) -> drawBackgroundColor $ V4 255 0 0 255
        (_, Just Unattempted) -> drawBackgroundColor $ V4 255 0 255 255
  loop




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



--   proc rfi -> do
--   battle <- renderBattle -< rfi
--   menus <- runSwont (error . show) $ battleMenu Nothing -< rfi
--   returnA -< battle <> menus

data HeroKey
  = Hero1
  | Hero2
  | Hero3
  | Hero4
  deriving (Eq, Ord, Show, Enum, Bounded)

data FighterId
  = HeroKey HeroKey
  | Menu
  deriving (Eq, Ord, Show)

data BattleMessage a where
  DoAction :: BattleMessage (BattleAction, Maybe FighterId)

deriving instance Eq (BattleMessage a)


menuObject :: FighterId -> ObjSF BattleMessage Void FighterId BattleFighter
menuObject owner =
  proc oi -> do
    sw <- getSwont $ battleMenu Nothing -< (oi_fi oi, oi_everyone oi)
      -- (\(action, target) ->  constant $
      --   ObjectOutput
      --     { oo_commands = pure Die
      --     , oo_outbox = [(owner, SomeMsg DoAction (action, target))]
      --     , oo_state = menuFighter
      --     , oo_render = mempty
      --     }) $ battleMenu Nothing -< (oi_fi oi, oi_everyone oi)
    returnA -<
      case sw of
        Left o  ->
          ObjectOutput
            { oo_commands = mempty
            , oo_outbox = mempty
            , oo_state = menuFighter
            , oo_render = o
            }
        Right r ->
          ObjectOutput
            { oo_commands = pure Die
            , oo_outbox = [(owner, SomeMsg DoAction r)]
            , oo_state = menuFighter
            , oo_render = mempty
            }


initialize :: Ord k => s -> ObjSF m c k s -> ObjSF m c k s
initialize s sf = proc oi -> do
  start <- now () -< ()
  oo <- sf -< event id (const $ #oi_everyone . at (oi_self oi) . _Just .~ s) start oi
  returnA -< event id (const $ #oo_state .~ s) start oo

heroHandler :: ObjSF BattleMessage Void FighterId BattleFighter
heroHandler = foreverSwont $ do
  !_ <- traceM "hello"
  pos <- get (bp_pos . oi_state) $ drawMe 0
  (action, target) <- swont $ proc oi -> do
    me <- drawMe 0 -< oi
    let ev = asum $ fmap (Event . snd) $ oie_mailbox (oi_inbox oi) DoAction
    returnA -< (me, ev)
  case action of
    Defend -> do
      let dur = 2
      dswont $ proc oi -> do
        end <- after dur () -< ()
        oo <- drawMe (-20) -< oi
        returnA -< ( oo
                        & #oo_commands <>~ [Spawn (Just Menu) undefined $ menuObject $ HeroKey Hero1 ]
                   , end)
      !_ <- traceM "all done"
      pure ()

  where
    drawMe :: V2 Double -> ObjSF BattleMessage Void FighterId BattleFighter
    drawMe offset = proc oi -> do
      returnA -< ObjectOutput
        { oo_state = oi_state oi
        , oo_commands = mempty
        , oo_outbox = mempty
        , oo_render = drawFilledRect (V4 0 0 0 255)
            $ Rectangle (P $ (bp_pos $ oi_state oi) + offset) 10
        }




game :: SF RawFrameInfo Renderable
game = testRouter


testRouter :: SF RawFrameInfo Renderable
testRouter = proc rfi -> do
  cc <- router @BattleMessage @Void @FighterId @BattleFighter undefined (\case) $ ObjectMap mempty $ M.fromList
    [
      (HeroKey Hero1, (horton, heroHandler))
    , (HeroKey Hero2, (skalp, proc oi -> do
        let col = V4 0 0 0 255
        returnA -< ObjectOutput
          { oo_render = drawFilledRect col
            $ Rectangle (P $ 400) 10
          , oo_state = oi_state oi
          , oo_outbox = mempty
          , oo_commands = mempty
          }
      ))
    , (Menu, (menuFighter , menuObject $ HeroKey Hero1))
    ] -< rfi
  returnA -< foldMap oo_render cc

