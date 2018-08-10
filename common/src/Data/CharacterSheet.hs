{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
module Data.CharacterSheet where

import Data.Functor.Classes
import Data.Functor.Identity
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)

data Abilities a = Abilities { str :: a
                             , dex :: a
                             , con :: a
                             , wis :: a
                             , int :: a
                             , cha :: a
                             }
                             deriving (Functor, Foldable, Traversable, Show)


abilityMod :: Int -> Int
abilityMod score = (score - 10) `div` 2

data ClassData a = ClassData { level :: a
                             , bab :: a
                             , fortitude :: a
                             , reflex :: a
                             , will :: a
                             }
                             deriving (Functor, Foldable, Traversable, Show)

data Ability = Strength
             | Dexterity
             | Constitution
             | Wisdom
             | Intelligence
             | Charisma
             deriving (Show)

shortName :: Ability -> Text
shortName Strength = "Str"
shortName Dexterity = "Dex"
shortName Constitution = "Con"
shortName Wisdom = "Wis"
shortName Intelligence = "Int"
shortName Charisma = "Cha"

data Skill = Skill { skillName :: Text
                   , isClassSkill :: Bool
                   , abilityType :: Ability
                   , skillRanks :: Int
                   , skillMod :: Int
                   }

pathfinderSkills :: Map Text Ability
pathfinderSkills = M.fromList [ ("Acrobatics", Dexterity)
                              , ("Appraise", Wisdom)
                              , ("Bluff", Charisma)
                              , ("Climb", Strength)
                              , ("Craft", Intelligence)
                              , ("Diplomacy", Charisma)
                              , ("Disable Device", Dexterity)
                              , ("Disguise", Charisma)
                              , ("Escape Artist", Dexterity)
                              , ("Fly", Dexterity)
                              , ("Handle Animal", Charisma)
                              , ("Heal", Wisdom)
                              , ("Intimidate", Charisma)
                              , ("Knowledge (arcana)",  Intelligence)
                              , ("Knowledge (dungeoneering)", Intelligence)
                              , ("Knowledge (engineering)", Intelligence)
                              , ("Knowledge (geography)", Intelligence)
                              , ("Knowledge (history)", Intelligence)
                              , ("Knowledge (local)", Intelligence)
                              , ("Knowledge (nature)", Intelligence)
                              , ("Knowledge (nobility)", Intelligence)
                              , ("Knowledge (planes)", Intelligence)
                              , ("Knowledge (religion)", Intelligence)
                              , ("Linguistics", Intelligence)
                              , ("Perception", Wisdom)
                              , ("Perform", Charisma)
                              , ("Profession", Wisdom)
                              , ("Ride", Dexterity)
                              , ("Sense Motive", Wisdom)
                              , ("Sleight of Hand", Dexterity)
                              , ("Spellcraft", Intelligence)
                              , ("Stealth", Dexterity)
                              , ("Survival", Wisdom)
                              , ("Swim", Strength)
                              , ("Use Magic Device", Charisma)
                              ]
