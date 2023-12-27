{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Battle.Types where

import Data.Map (Map)
import Engine.Types
import Data.Maybe


type BattleState = Map FighterId (Maybe BattleFighter)

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
  deriving (Eq, Ord, Show, Enum, Bounded)


data TimedHitResult = Unattempted | Flubbed | Good | Perfect
  deriving (Eq, Ord, Show, Enum, Bounded)

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

oi_state' :: Ord k => ObjectInput msg k (Maybe s) -> s
oi_state' = fromJust . oi_state

