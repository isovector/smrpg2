
module Game where

import Control.Monad
import Data.Monoid
import Engine.Types
import Engine.Drawing
import Engine.Utils
import Data.Foldable
import Data.List


handleClose :: SF RawFrameInfo (Event BattleMenu)
handleClose = proc rfi -> do
  att <- edge -< c_ok     $ fi_controls rfi
  def <- edge -< c_cancel $ fi_controls rfi
  item <- edge -< c_item $ fi_controls rfi
  spell <- edge -< c_spell $ fi_controls rfi
  returnA -< asum
    [ AttackMenu <$ att
    , DefendMenu <$ def
    , ItemMenu   <$ item
    , SpellMenu  <$ spell
    ]


selectParticipant :: BattleState -> (BattleParticipant -> Bool) -> BattleMenu -> Swont RawFrameInfo Renderable (Either BattleMenu BattleParticipant)
selectParticipant bs pred me = swont $ loopPre 0 $ proc (rfi, ix) -> do
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


  returnA -< ((drawFilledRect (V4 255 0 255 255) $ Rectangle (P $ bp_pos sel_part - 10) 10, asum [ev, fmap Left close] ), clamp 0 (len - 1) $ fromEvent id update_ix ix)
 where
  parts = sortOn (view _x . bp_pos) $ filter pred $ bs_participants bs
  len = length $ parts

menu :: Display a => [a] -> BattleMenu -> Swont RawFrameInfo Renderable (Either BattleMenu a)
menu opts me = swont $ loopPre 0 $ proc (rfi, ix) -> do
  close <- handleClose -< rfi
  let ev = do
        e <- close
        guard $ me == e
        pure $ Right $ opts !! ix

  up    <- edge -< c_up $ fi_controls rfi
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

data BattleAction = Attack | Defend | RunAway | UseItem Item BattleParticipant | UseSpell Spell
  deriving (Eq, Ord, Show)

instance Display BattleAction where
  display = show

instance Display BattleMenu where
  display = show

data BattleParticipant = BattleParticipant
  { bp_hp    :: Int
  , bp_team  :: Team
  , bp_color :: Color
  , bp_pos   :: V2 Double
  }
  deriving (Eq, Ord, Show)

data BattleState = BattleState
  { bs_participants :: [BattleParticipant]
  }
  deriving (Eq, Ord, Show)


mario :: BattleParticipant
mario = BattleParticipant 100 HeroTeam (V4 255 0 0 255) 300

mallow :: BattleParticipant
mallow = BattleParticipant 100 HeroTeam (V4 255 255 255 255) 400

state :: BattleState
state = BattleState [mario, mallow]

battleMenu :: Maybe BattleMenu -> Swont RawFrameInfo Renderable BattleAction
battleMenu m = do
  e <- case m of
    Nothing -> swont $ fmap ((mempty, ) . fmap Left) handleClose
    Just AttackMenu -> menu [Attack] AttackMenu
    Just DefendMenu -> menu [Defend, RunAway] DefendMenu
    Just ItemMenu   -> do
      mitem <- menu [minBound @Item .. maxBound] ItemMenu
      case mitem of
        Left  bm -> pure $ Left bm
        Right item -> do
          selectParticipant state (const True) ItemMenu >>= \case
            Left  m' -> pure $ Left m'
            Right who -> pure $ Right $ UseItem item who
    Just SpellMenu  -> fmap (fmap UseSpell) $ menu [minBound .. maxBound] SpellMenu
  case e of
    Right a -> pure a
    Left m' -> battleMenu $ Just m'

renderBattle :: SF RawFrameInfo Renderable
renderBattle = proc rfi -> do
  returnA -< mconcat $ do
    p <- bs_participants state
    pure $ drawFilledRect (bp_color p) $ Rectangle (P $ bp_pos p) 20




game :: SF RawFrameInfo Renderable
game = proc rfi -> do
  battle <- renderBattle -< rfi
  menus <- runSwont (error . show) $ battleMenu Nothing -< rfi
  returnA -< battle <> menus



--   proc (rfi, which) -> do
--   (att, _) <- pause mempty _ $ menu ["attack"] (arr $ c_ok . fi_controls) -< rfi
--   (def, _) <- pause mempty _ $ menu ["defend", "run away"] (arr $ c_cancel . fi_controls) -< rfi
--   returnA -< (, which) $ mconcat
--     [ att
--     , def
--     ]

