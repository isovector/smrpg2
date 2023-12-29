{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Battle.Types where

import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Void
import Engine.Router
import Engine.Types

type MSG = BattleMessage
type KEY = FighterId
type STATE = Maybe BattleFighter
type CMD = Void

type COMMAND = Command MSG CMD KEY STATE

type OI = ObjectInput MSG KEY STATE
type OO = ObjectOutput MSG CMD KEY STATE

type BattleState = Map FighterId STATE

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


data FighterSelection
  = SelectHero
  | SelectEnemy
  | SelectAnyone
  | NoSelection
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)


data TimedHitResult
  = Flubbed | Unattempted | Good | Perfect
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

data HeroKey
  = Hero1
  | Hero2
  | Hero3
  | Hero4
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

data FighterId
  = HeroKey HeroKey
  | EnemyKey
  | Menu
  | Ephemeral Int
  deriving (Eq, Ord, Show, Generic)

data BattleMessage a where
  DoAction       :: BattleMessage (BattleAction, Maybe KEY)
  DoDamage       :: BattleMessage Int
  DoHealing      :: BattleMessage Int
  DoMortalDamage :: BattleMessage ()

deriving instance Eq   (BattleMessage a)
deriving instance Ord  (BattleMessage a)
deriving instance Show (BattleMessage a)

data AttackResult = AttackResult
  { ar_messages :: [(FighterId, SomeMsg MSG)]
  , ar_commands :: [COMMAND]
  }

instance Semigroup AttackResult where
  AttackResult a1 a2 <> AttackResult b1 b2
    = AttackResult
        (a1 <> b1)
        (a2 <> b2)

instance Monoid AttackResult where
  mempty = AttackResult mempty mempty

oi_state' :: Ord k => ObjectInput msg k (Maybe s) -> s
oi_state' = fromJust . oi_state


battleRouter
  :: (forall x. Map KEY x -> KEY)
  -> (KEY -> CMD -> Endo (ObjectMap MSG KEY STATE (SF OI OO)))
  -> ObjectMap MSG KEY STATE (SF OI OO)
  -> SF RawFrameInfo (ObjectMap MSG KEY STATE OO)
battleRouter =
  router @MSG
         @CMD
         @KEY
         @STATE

type BattleScript = forall r. Swont r OI OO AttackResult

