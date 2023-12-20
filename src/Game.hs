
module Game where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Engine.Types
import Engine.Drawing
import Engine.Utils
import Data.Foldable
import Data.List
import Control.Monad.Except


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


selectParticipant
  :: BattleState
  -> (BattleParticipant -> Bool)
  -> BattleMenu
  -> ExceptT BattleMenu (Swont RawFrameInfo Renderable) BattleParticipant
selectParticipant bs pred me = ExceptT $ swont $ loopPre 0 $ proc (rfi, ix) -> do
  let sel_part = parts !! ix

  close <- handleClose -< rfi
  let ev = do
        e <- close
        guard $ me == e
        pure $ Right sel_part

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
 where
  parts = sortOn (view _x . bp_pos) $ filter pred $ bs_participants bs
  len = length $ parts


menu :: Display a => [a] -> BattleMenu -> ExceptT BattleMenu (Swont RawFrameInfo Renderable) a
menu opts me = ExceptT $ swont $ loopPre 0 $ proc (rfi, ix) -> do
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

data Team = HeroTeam | EnemyTeam
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

needsSelection :: BattleAction -> ParticipantSelection
needsSelection Attack = SelectEnemy
needsSelection Defend = NoSelection
needsSelection RunAway = NoSelection
needsSelection (UseItem Item_TestItem) = SelectHero
needsSelection (UseSpell Spell_TestSpell) = SelectEnemy

toPredicate :: ParticipantSelection -> BattleParticipant -> Bool
toPredicate SelectEnemy = (== EnemyTeam) . bp_team
toPredicate SelectHero = (== HeroTeam) . bp_team
toPredicate SelectAnyone = const True
toPredicate NoSelection = const False

instance Display BattleAction where
  display = show

instance Display BattleMenu where
  display = show

data BattleParticipant = BattleParticipant
  { bp_name  :: String
  , bp_hp    :: Int
  , bp_team  :: Team
  , bp_color :: Color
  , bp_pos   :: V2 Double
  }
  deriving (Eq, Ord, Show)

data BattleState = BattleState
  { bs_participants :: [BattleParticipant]
  }
  deriving (Eq, Ord, Show)


horton :: BattleParticipant
horton = BattleParticipant "Horton" 100 HeroTeam (V4 255 0 0 255) 300


skalp :: BattleParticipant
skalp = BattleParticipant "Skalp" 100 HeroTeam (V4 255 255 255 255) 400


baddie :: BattleParticipant
baddie = BattleParticipant "Baddie" 100 EnemyTeam (V4 0 0 0 255) $ V2 400 200


state :: BattleState
state = BattleState [horton, skalp, baddie]

data ParticipantSelection
  = SelectHero
  | SelectEnemy
  | SelectAnyone
  | NoSelection
  deriving (Eq, Ord, Show, Enum, Bounded)


data TimedHitResult = Unattempted | Flubbed | Good | Perfect
  deriving (Eq, Ord, Show, Enum, Bounded)

timedHit :: BattleMenu -> SF (RawFrameInfo, Event a) (Event TimedHitResult)
timedHit bm = loopPre (0, noEvent, noEvent) $ proc ((rfi, raw_ev), (last_attempt, last_ok, seen)) -> do
  let cooldown = 0.3
      grace    = 0.15
      perfect  = 0.05
  now <- time -< ()
  let ev = now <$ raw_ev
  attempt_press <- edge -< menuButton bm $ fi_controls rfi
  let is_attempt = isEvent attempt_press
  let attempt_ev = now <$ attempt_press
  let ok_ev =
        case is_attempt && now - last_attempt >= cooldown of
          True  -> attempt_ev
          False -> noEvent
  returnA
    -< ( case traceShowId (seen, last_ok) of
           (Event real, Event hit)
             | abs (real - hit) <= perfect -> Event Perfect
             | abs (real - hit) <= grace   -> Event Good
             | otherwise                   -> Event Flubbed
           (Event real, _)
             | now >= real + grace         -> Event Unattempted
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




battleMenu :: Maybe BattleMenu -> Swont RawFrameInfo Renderable (BattleAction, Maybe BattleParticipant)
battleMenu m = do
  result <- runExceptT $ do
    (mode, action) <- case m of
      Nothing -> ExceptT $ swont $ fmap ((mempty, ) . fmap Left) handleClose
      Just AttackMenu -> fmap (AttackMenu, ) $ menu [Attack] AttackMenu
      Just DefendMenu -> fmap (DefendMenu, ) $ menu [Defend, RunAway] DefendMenu
      Just ItemMenu   -> fmap ((ItemMenu, )  . UseItem)  $ menu [minBound @Item  .. maxBound] ItemMenu
      Just SpellMenu  -> fmap ((SpellMenu, ) . UseSpell) $ menu [minBound @Spell .. maxBound] SpellMenu
    who <- case needsSelection action of
      NoSelection -> pure Nothing
      sel         -> fmap Just $ selectParticipant state (toPredicate sel) mode
    pure (action, who)
  either (battleMenu . Just) pure result


renderBattle :: SF RawFrameInfo Renderable
renderBattle = proc rfi -> do
  returnA -< mconcat $ do
    p <- bs_participants state
    pure $ drawFilledRect (bp_color p) $ Rectangle (P $ bp_pos p) 20


game :: SF RawFrameInfo Renderable
game = testTimedHits

--   proc rfi -> do
--   battle <- renderBattle -< rfi
--   menus <- runSwont (error . show) $ battleMenu Nothing -< rfi
--   returnA -< battle <> menus

