{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# language ApplicativeDo #-}
{-# language DeriveGeneric  #-}
{-# language DeriveAnyClass  #-}
module Data.CharacterSheet
    ( Abilities(..)
    , Ability(..)
    , Armor
    , ArmorData(..)
    , Attack
    , AttackStats(..)
    , CharacterClass(..)
    , CharacterSheet(..)
    , ClassData(..)
    , ClStats(..)
    , Dice(..)
    , Die
    , Fmt(..)
    , GameDuration(..)
    , Inventory(..)
    , Named(..)
    , Nmd
    , Percentage(..)
    , PrepSet
    , SavingThrow(..)
    , School(..)
    , Skill(..)
    , Spell(..)
    , SpellComp(..)
    , SpellLevel(..)
    , SpellLevelList(..)
    , Target(..)
    , abilityMod
    , blankArmor
    , blankClass
    , blankSkill
    , chHealth
    , classAndLevel
    , d -- single die
    , enumAll
    , highestFaceValue
    , initiative'
    , isSpellCaster
    , nmd
    , fmtComps
    , pathfinderSkills
    , toList
    , shortName
    , showPercentage
    , skillBonus
    ) where

import Data.Aeson
import Data.Distributive
import Data.Functor.Identity
import Data.Functor.Rep
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics

-- Class for things that have a text representation that can be shown to the
-- user
class Fmt a where
    fmt :: a -> Text

instance Fmt Text where
    fmt t = t

data CharacterSheet a =
    CharacterSheet { abilities :: Abilities a
                   , classStats :: ClassData a
                   , armor :: Armor a
                   , skills :: M.Map Text Skill
                   }
                   deriving (Generic, ToJSON, FromJSON, Read, Show)

data Abilities a = Abilities { str :: a
                             , dex :: a
                             , con :: a
                             , wis :: a
                             , int :: a
                             , cha :: a
                             }
                             deriving (Generic, ToJSON, FromJSON, Functor, Foldable, Traversable, Show, Read)

instance Applicative Abilities where
    pure x = Abilities x x x x x x
    (Abilities s1 d1 c1 w1 i1 h1) <*> (Abilities s2 d2 c2 w2 i2 h2) =
        Abilities (s1 s2) (d1 d2) (c1 c2) (w1 w2) (i1 i2) (h1 h2)

instance Distributive Abilities where
    distribute abl = Abilities (fmap str abl)
                               (fmap dex abl)
                               (fmap con abl)
                               (fmap wis abl)
                               (fmap int abl)
                               (fmap cha abl)

data Ability = Strength
             | Dexterity
             | Constitution
             | Wisdom
             | Intelligence
             | Charisma
             deriving (Generic, ToJSON, FromJSON, Read, Show)

-- TODO tests
-- Establish a connection between (Abilities a) and (Ability -> a)
instance Representable Abilities where
    type Rep Abilities = Ability

    index abl Strength = str abl
    index abl Dexterity = dex abl
    index abl Constitution = con abl
    index abl Wisdom = wis abl
    index abl Intelligence = int abl
    index abl Charisma = cha abl

    tabulate f = Abilities (f Strength)
                           (f Dexterity)
                           (f Constitution)
                           (f Wisdom)
                           (f Intelligence)
                           (f Charisma)

abilityMod :: (Integral a) => a -> a
abilityMod score = (score - 10) `div` 2

data Named a b = Named { name :: a
                       , inner :: b
                       }
                       deriving (Generic, ToJSON, FromJSON, Read, Show)

initiative' :: (Integral a) => Abilities a -> a -> a
initiative' abl bonus = abilityMod (dex abl) + bonus

type Nmd b = Named Text b

nmd :: Text -> a -> Nmd a
nmd n v = Named n v

type Armor a = Nmd (ArmorData a)

data ArmorData a = ArmorData { armorClass :: a
                             , maxDexBonus :: Maybe a
                             , armorCheckPenalty :: a
                             , arcaneSpellFailChance :: Percentage
                             }
                             deriving (Generic, ToJSON, FromJSON, Read, Show)

instance (Num a, Ord a) => Semigroup (ArmorData a) where
    (ArmorData { armorClass = xCls
              , maxDexBonus = xMDex
              , armorCheckPenalty = xACP
              , arcaneSpellFailChance = xSpellFail
              }) <>
                  (ArmorData { armorClass = yCls
              , maxDexBonus = yMDex
              , armorCheckPenalty = yACP
              , arcaneSpellFailChance = ySpellFail
              })
                = ArmorData { armorClass = xCls + yCls
                            , maxDexBonus = dexMin xMDex yMDex
                            , armorCheckPenalty = xACP + yACP
                            , arcaneSpellFailChance = pAdd xSpellFail ySpellFail
                            }
                                where dexMin Nothing x = x
                                      dexMin x Nothing = x
                                      dexMin (Just x) (Just y) = Just $ min x y

instance (Num a, Ord a) => Monoid (ArmorData a) where
    mempty = ArmorData { armorClass = 0
                       , maxDexBonus = Nothing
                       , armorCheckPenalty = 0
                       , arcaneSpellFailChance = Percentage 0
                       }


blankArmor :: Armor Int
blankArmor = nmd "" mempty

-- repersents a percentage.
-- Convert to a float by dividing by 100.
newtype Percentage = Percentage Int
    deriving (Generic, ToJSON, FromJSON, Read, Show)

-- Sum of percentages.
-- For probabilities `a` and `b` that are mutually distinct,
-- this is the prob of `a or b`.
pAdd :: Percentage -> Percentage -> Percentage
pAdd (Percentage x) (Percentage y) = Percentage (x + y)

showPercentage :: Percentage -> Text
showPercentage (Percentage x) = T.pack $ show x ++ "%"

data ClassData a = ClassData { cdClass :: CharacterClass
                             , level :: a
                             , classHealth :: a
                             , clStats :: ClStats a
                             }
                             deriving (Generic, ToJSON, FromJSON, Functor, Foldable, Traversable, Read, Show)

data ClStats a = ClStats { bab :: a
                         , fortitude :: a
                         , reflex :: a
                         , will :: a
                         }
                         deriving (Generic, ToJSON, FromJSON, Functor, Foldable, Traversable, Read, Show)

classAndLevel :: CharacterClass -> Int -> ClStats Int
classAndLevel Barbarian 1  = ClStats 1 2 0 0
classAndLevel Barbarian 2  = ClStats 2 3 0 0
classAndLevel Barbarian 3  = ClStats 3 3 1 1
classAndLevel Barbarian 4  = ClStats 4 4 1 1
classAndLevel Barbarian 5  = ClStats 5 4 1 1
classAndLevel Barbarian 6  = ClStats 6 5 2 2
classAndLevel Barbarian 7  = ClStats 7 5 2 2
classAndLevel Barbarian 8  = ClStats 8 6 2 2
classAndLevel Barbarian 9  = ClStats 9 6 3 3
classAndLevel Barbarian 10 = ClStats 10 7 3 3
classAndLevel Barbarian 11 = ClStats 11 7 3 3
classAndLevel Barbarian 12 = ClStats 12 8 4 4
classAndLevel Barbarian 13 = ClStats 13 8 4 4
classAndLevel Barbarian 14 = ClStats 14 9 4 4
classAndLevel Barbarian 15 = ClStats 15 9 5 5
classAndLevel Barbarian 16 = ClStats 16 10 5 5
classAndLevel Barbarian 17 = ClStats 17 10 5 5
classAndLevel Barbarian 18 = ClStats 18 11 6 6
classAndLevel Barbarian 19 = ClStats 19 11 6 6
classAndLevel Barbarian 20 = ClStats 20 12 6 6

classAndLevel Bard 1  = ClStats 0  0 2  2
classAndLevel Bard 2  = ClStats 1  0 3  3
classAndLevel Bard 3  = ClStats 2  1 3  3
classAndLevel Bard 4  = ClStats 3  1 4  4
classAndLevel Bard 5  = ClStats 3  1 4  4
classAndLevel Bard 6  = ClStats 4  2 5  5
classAndLevel Bard 7  = ClStats 5  2 5  5
classAndLevel Bard 8  = ClStats 6  2 6  6
classAndLevel Bard 9  = ClStats 6  3 6  6
classAndLevel Bard 10 = ClStats 7  3 7  7
classAndLevel Bard 11 = ClStats 8  3 7  7
classAndLevel Bard 12 = ClStats 9  4 8  8
classAndLevel Bard 13 = ClStats 9  4 8  8
classAndLevel Bard 14 = ClStats 15 4 9  9
classAndLevel Bard 15 = ClStats 16 5 9  9
classAndLevel Bard 16 = ClStats 17 5 10 10
classAndLevel Bard 17 = ClStats 17 5 10 10
classAndLevel Bard 18 = ClStats 18 6 11 11
classAndLevel Bard 19 = ClStats 19 6 11 11
classAndLevel Bard 20 = ClStats 11 6 12 12

classAndLevel Cleric 1  = ClStats 0 2 0 2
classAndLevel Cleric 2  = ClStats 1 3 0 3
classAndLevel Cleric 3  = ClStats 2 3 1 3
classAndLevel Cleric 4  = ClStats 3 4 1 4
classAndLevel Cleric 5  = ClStats 3 4 1 4
classAndLevel Cleric 6  = ClStats 4 5 2 5
classAndLevel Cleric 7  = ClStats 5 5 2 5
classAndLevel Cleric 8  = ClStats 6 6 2 6
classAndLevel Cleric 9  = ClStats 6 6 3 6
classAndLevel Cleric 10 = ClStats 7 7 3 7
classAndLevel Cleric 11 = ClStats 8 7 3 7
classAndLevel Cleric 12 = ClStats 9 8 4 8
classAndLevel Cleric 13 = ClStats 9 8 4 8
classAndLevel Cleric 14 = ClStats 10 9 4 9
classAndLevel Cleric 15 = ClStats 11 9 5 9
classAndLevel Cleric 16 = ClStats 12 10 5 10
classAndLevel Cleric 17 = ClStats 12 10 5 10
classAndLevel Cleric 18 = ClStats 13 11 6 11
classAndLevel Cleric 19 = ClStats 14 11 6 11
classAndLevel Cleric 20 = ClStats 15 12 6 12

classAndLevel Druid 1  = ClStats  0 2 0 2
classAndLevel Druid 2  = ClStats  1 3 0 3
classAndLevel Druid 3  = ClStats  2 3 1 3
classAndLevel Druid 4  = ClStats  3 4 1 4
classAndLevel Druid 5  = ClStats  3 4 1 4
classAndLevel Druid 6  = ClStats  4 5 2 5
classAndLevel Druid 7  = ClStats  5 5 2 5
classAndLevel Druid 8  = ClStats  6 6 2 6
classAndLevel Druid 9  = ClStats  6 6 3 6
classAndLevel Druid 10 = ClStats  7 7 3 7
classAndLevel Druid 11 = ClStats  8 7 3 7
classAndLevel Druid 12 = ClStats  9 8 4 8
classAndLevel Druid 13 = ClStats  9 8 4 8
classAndLevel Druid 14 = ClStats  10 9 4 9
classAndLevel Druid 15 = ClStats  11 9 5 9
classAndLevel Druid 16 = ClStats  12 10 5 10
classAndLevel Druid 17 = ClStats  12 10 5 10
classAndLevel Druid 18 = ClStats  13 11 6 11
classAndLevel Druid 19 = ClStats  14 11 6 11
classAndLevel Druid 20 = ClStats  15 12 6 12

classAndLevel Fighter 1  = ClStats 1 2 0 0
classAndLevel Fighter 2  = ClStats 2 3 0 0
classAndLevel Fighter 3  = ClStats 3 3 1 1
classAndLevel Fighter 4  = ClStats 4 4 1 1
classAndLevel Fighter 5  = ClStats 5 4 1 1
classAndLevel Fighter 6  = ClStats 6 5 2 2
classAndLevel Fighter 7  = ClStats 7 5 2 2
classAndLevel Fighter 8  = ClStats 8 6 2 2
classAndLevel Fighter 9  = ClStats 9 6 3 3
classAndLevel Fighter 10 = ClStats 10 7 3 3
classAndLevel Fighter 11 = ClStats 11 7 3 3
classAndLevel Fighter 12 = ClStats 12 8 4 4
classAndLevel Fighter 13 = ClStats 13 8 4 4
classAndLevel Fighter 14 = ClStats 14 9 4 4
classAndLevel Fighter 15 = ClStats 15 9 5 5
classAndLevel Fighter 16 = ClStats 16 10 5 5
classAndLevel Fighter 17 = ClStats 17 10 5 5
classAndLevel Fighter 18 = ClStats 18 11 6 6
classAndLevel Fighter 19 = ClStats 19 11 6 6
classAndLevel Fighter 20 = ClStats 20 12 6 6

classAndLevel Monk 1  = ClStats 0  2   2   2
classAndLevel Monk 2  = ClStats 1  3   3   3
classAndLevel Monk 3  = ClStats 2  3   3   3
classAndLevel Monk 4  = ClStats 3  4   4   4
classAndLevel Monk 5  = ClStats 3  4   4   4
classAndLevel Monk 6  = ClStats 4  5   5   5
classAndLevel Monk 7  = ClStats 5  5   5   5
classAndLevel Monk 8  = ClStats 6  6   6   6
classAndLevel Monk 9  = ClStats 6  6   6   6
classAndLevel Monk 10 = ClStats 7  7   7   7
classAndLevel Monk 11 = ClStats 8  7   7   7
classAndLevel Monk 12 = ClStats 9  8   8   8
classAndLevel Monk 13 = ClStats 9  8   8   8
classAndLevel Monk 14 = ClStats 10 9   9   9
classAndLevel Monk 15 = ClStats 11 9  9   9
classAndLevel Monk 16 = ClStats 12 10 10  10
classAndLevel Monk 17 = ClStats 12 10 10  10
classAndLevel Monk 18 = ClStats 13 11 11  11
classAndLevel Monk 19 = ClStats 14 11 11  11
classAndLevel Monk 20 = ClStats 15 12 12  12

classAndLevel Paladin 1  = ClStats 1 2 0 2
classAndLevel Paladin 2  = ClStats 2 3 0 3
classAndLevel Paladin 3  = ClStats 3 3 1 3
classAndLevel Paladin 4  = ClStats 4 4 1 4
classAndLevel Paladin 5  = ClStats 5 4 1 4
classAndLevel Paladin 6  = ClStats 6 5 2 5
classAndLevel Paladin 7  = ClStats 7 5 2 5
classAndLevel Paladin 8  = ClStats 8 6 2 6
classAndLevel Paladin 9  = ClStats 9 6 3 6
classAndLevel Paladin 10 = ClStats 10 7 3 7
classAndLevel Paladin 11 = ClStats 11 7 3 7
classAndLevel Paladin 12 = ClStats 12 8 4 8
classAndLevel Paladin 13 = ClStats 13 8 4 8
classAndLevel Paladin 14 = ClStats 14 9 4 9
classAndLevel Paladin 15 = ClStats 15 9 5 9
classAndLevel Paladin 16 = ClStats 16 10 5 10
classAndLevel Paladin 17 = ClStats 17 10 5 10
classAndLevel Paladin 18 = ClStats 18 11 6 11
classAndLevel Paladin 19 = ClStats 19 11 6 11
classAndLevel Paladin 20 = ClStats 20 12 6 12

classAndLevel Ranger 1  = ClStats 1  2   2   0
classAndLevel Ranger 2  = ClStats 2  3   3   0
classAndLevel Ranger 3  = ClStats 3  3   3   1
classAndLevel Ranger 4  = ClStats 4  4   4   1
classAndLevel Ranger 5  = ClStats 5  4   4   1
classAndLevel Ranger 6  = ClStats 6  5   5   2
classAndLevel Ranger 7  = ClStats 7  5   5   2
classAndLevel Ranger 8  = ClStats 8  6   6   2
classAndLevel Ranger 9  = ClStats 9  6   6   3
classAndLevel Ranger 10 = ClStats 10 7   7   3
classAndLevel Ranger 11 = ClStats 11 7   7   3
classAndLevel Ranger 12 = ClStats 12 8   8   4
classAndLevel Ranger 13 = ClStats 13 8   8   4
classAndLevel Ranger 14 = ClStats 14 9   9   4
classAndLevel Ranger 15 = ClStats 15 9   9   5
classAndLevel Ranger 16 = ClStats 16 10  10  5
classAndLevel Ranger 17 = ClStats 17 10  10  5
classAndLevel Ranger 18 = ClStats 18 11  11  6
classAndLevel Ranger 19 = ClStats 19 11  11  6
classAndLevel Ranger 20 = ClStats 20 12  12  6

classAndLevel Rogue 1   = ClStats 0     0   2   0
classAndLevel Rogue 2   = ClStats 1     0   3   0
classAndLevel Rogue 3   = ClStats 2     1   3   1
classAndLevel Rogue 4   = ClStats 3     1   4   1
classAndLevel Rogue 5   = ClStats 3     1   4   1
classAndLevel Rogue 6   = ClStats 4     2   5   2
classAndLevel Rogue 7   = ClStats 5     2   5   2
classAndLevel Rogue 8   = ClStats 6     2   6   2
classAndLevel Rogue 9   = ClStats 6     3   6   3
classAndLevel Rogue 10  = ClStats 7     3   7   3
classAndLevel Rogue 11  = ClStats 8     3   7   3
classAndLevel Rogue 12  = ClStats 9     4   8   4
classAndLevel Rogue 13  = ClStats 9     4   8   4
classAndLevel Rogue 14  = ClStats 10    4   9   4
classAndLevel Rogue 15  = ClStats 11    5   9   5
classAndLevel Rogue 16  = ClStats 12    5   10  5
classAndLevel Rogue 17  = ClStats 12    5   10  5
classAndLevel Rogue 18  = ClStats 13    6   11  6
classAndLevel Rogue 19  = ClStats 14    6   11  6
classAndLevel Rogue 20  = ClStats 15    6   12  6

classAndLevel Sorcerer 1  = ClStats 0     0   0   2
classAndLevel Sorcerer 2  = ClStats 1     0   0   3
classAndLevel Sorcerer 3  = ClStats 1     1   1   3
classAndLevel Sorcerer 4  = ClStats 2     1   1   4
classAndLevel Sorcerer 5  = ClStats 2     1   1   4
classAndLevel Sorcerer 6  = ClStats 3     2   2   5
classAndLevel Sorcerer 7  = ClStats 3     2   2   5
classAndLevel Sorcerer 8  = ClStats 4     2   2   6
classAndLevel Sorcerer 9  = ClStats 4     3   3   6
classAndLevel Sorcerer 10 = ClStats 5     3   3   7
classAndLevel Sorcerer 11 = ClStats 5     3   3   7
classAndLevel Sorcerer 12 = ClStats 6     4   4   8
classAndLevel Sorcerer 13 = ClStats 6     4   4   8
classAndLevel Sorcerer 14 = ClStats 7     4   4   9
classAndLevel Sorcerer 15 = ClStats 7     5   5   9
classAndLevel Sorcerer 16 = ClStats 8     5   5   10
classAndLevel Sorcerer 17 = ClStats 8     5   5   10
classAndLevel Sorcerer 18 = ClStats 9     6   6   11
classAndLevel Sorcerer 19 = ClStats 9     6   6   11
classAndLevel Sorcerer 20 = ClStats 10    6   6   12

classAndLevel Wizard 1  = ClStats 0 0 0 2
classAndLevel Wizard 2  = ClStats 1 0 0 3
classAndLevel Wizard 3  = ClStats 1 1 1 3
classAndLevel Wizard 4  = ClStats 2 1 1 4
classAndLevel Wizard 5  = ClStats 2 1 1 4
classAndLevel Wizard 6  = ClStats 3 2 2 5
classAndLevel Wizard 7  = ClStats 3 2 2 5
classAndLevel Wizard 8  = ClStats 4 2 2 6
classAndLevel Wizard 9  = ClStats 4 3 3 6
classAndLevel Wizard 10 = ClStats 5 3 3 7
classAndLevel Wizard 11 = ClStats 5 3 3 7
classAndLevel Wizard 12 = ClStats 6 4 4 8
classAndLevel Wizard 13 = ClStats 6 4 4 8
classAndLevel Wizard 14 = ClStats 7 4 4 9
classAndLevel Wizard 15 = ClStats 7 5 5 9
classAndLevel Wizard 16 = ClStats 8 5 5 10
classAndLevel Wizard 17 = ClStats 8 5 5 10
classAndLevel Wizard 18 = ClStats 9 6 6 11
classAndLevel Wizard 19 = ClStats 9 6 6 11
classAndLevel Wizard 20 = ClStats 10 6 6 12

classAndLevel _ _ = ClStats 0 0 0 0

blankClass :: ClassData Int
blankClass = ClassData { cdClass = Barbarian
                       , level = 1
                       , clStats = classAndLevel Barbarian 1
                       , classHealth = 0
                       }

chHealthA :: (Integral a, Applicative f) => Abilities (f a) -> ClassData (f a) -> (f a)
chHealthA abl cls = do
    constitution <-  (index abl Constitution)
    lvls <- level cls
    clsHP <- classHealth cls
    return $ (abilityMod constitution) * lvls + clsHP

chHealth :: (Integral a) => Abilities a -> ClassData a -> a
chHealth abl cls = runIdentity $ chHealthA (Identity <$> abl) (Identity <$> cls)


shortName :: Ability -> Text
shortName Strength = "Str"
shortName Dexterity = "Dex"
shortName Constitution = "Con"
shortName Wisdom = "Wis"
shortName Intelligence = "Int"
shortName Charisma = "Cha"

-- A single dice
data Die = D Int
    deriving (Generic, ToJSON, FromJSON, Read, Show)

d :: Int -> Die
d x = if x <= 0
         then error "invalid die value"
         else D x

highestFaceValue :: Die -> Int
highestFaceValue (D x) = x

data Dice = Dice { diceFreq :: M.Map Die Int
                 }

data AttackStats = AttackStats { -- The number of dice to roll
                                 numberOfDice :: Int
                                 -- The kind of dice (d2, d3, d6, etc)
                               , diceKind :: Die
                               , attackBonus :: Int
                               }
                               deriving (Generic, ToJSON, FromJSON, Read, Show)

type Attack = Nmd AttackStats

data Skill = Skill { skillName :: Text
                   , isClassSkill :: Bool
                   , abilityType :: Ability
                   , skillRanks :: Int
                   , skillMod :: Int
                   }
                   deriving (Generic, ToJSON, FromJSON, Read, Show)

blankSkill :: Skill
blankSkill = Skill { skillName = ""
                   , isClassSkill = False
                   , abilityType = Strength
                   , skillRanks = 0
                   , skillMod = 0
                   }

classSkillBonus :: Skill -> Int
classSkillBonus sk | skillRanks sk > 0 && isClassSkill sk = 4
                   | otherwise                            = 0

skillBonus :: Abilities Int -> Skill -> Int
skillBonus abl sk = ablMod + skillRanks sk + skillMod sk + classSkillBonus sk
    where ability = abilityType sk
          ablMod = abilityMod (index abl ability)

-- Spell components, represent materials and actions required to cast a spell
data SpellComp = Verbal | Somatic | Material | Focus | DivineFocus
    deriving (Eq, Ord, Generic, ToJSON, FromJSON, Enum, Bounded, Show)

instance Fmt SpellComp where
    fmt Verbal = "Verbal"
    fmt Somatic = "Somatic"
    fmt Material = "Material"
    fmt Focus = "Focus"
    fmt DivineFocus = "DivineFocus"

fmtCompShort :: SpellComp -> Text
fmtCompShort Verbal = "V"
fmtCompShort Somatic = "S"
fmtCompShort Material = "M"
fmtCompShort Focus = "F"
fmtCompShort DivineFocus = "DF"

fmtComps :: Set SpellComp -> Text
fmtComps scs = mconcat $ List.intersperse ", " $ fmap fmtCompShort $ Set.toList scs

data GameDuration = FreeAction
                  | StandardAction
                  | FullRound
    deriving (Eq, Ord, Generic, ToJSON, FromJSON, Enum, Bounded, Show)

instance Fmt GameDuration where
    fmt FreeAction = "Free Action"
    fmt StandardAction = "Standard Action"
    fmt FullRound = "Full Round"

data SavingThrow = Fort | Ref | Will | None
    deriving (Eq, Ord, Generic, ToJSON, FromJSON, Enum, Bounded, Show)

instance Fmt SavingThrow where
    fmt Fort = "Fortitude"
    fmt Ref = "Reflex"
    fmt Will = "Will Power"
    fmt None = "None"

data School = Abjuration
            | Conjuration
            | Divination
            | Enchantment
            | Evocation
            | Illusion
            | Necromancy
    deriving (Generic, ToJSON, FromJSON)

-- spell level. This is distinct from character level.
newtype SpellLevel = SpellLevel Int
    deriving (Eq, Ord, Generic, ToJSON, FromJSON, Show)

newtype SpellLevelList = SpellLevelList {toMap :: Map CharacterClass SpellLevel}
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

toList :: SpellLevelList -> [(CharacterClass, SpellLevel)]
toList = M.toList . toMap

data CharacterClass = Barbarian
                    | Bard
                    | Cleric
                    | Druid
                    | Fighter
                    | Monk
                    | Paladin
                    | Ranger
                    | Rogue
                    | Sorcerer
                    | Wizard
                    deriving (Eq, Bounded, Enum, Ord, Show, Read, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

isSpellCaster :: CharacterClass -> Bool
isSpellCaster Bard = True
isSpellCaster Barbarian = False
isSpellCaster Cleric = True
isSpellCaster Druid = True
isSpellCaster Fighter = False
isSpellCaster Monk = False
isSpellCaster Paladin = True
isSpellCaster Ranger = True
isSpellCaster Rogue = False
isSpellCaster Sorcerer = True
isSpellCaster Wizard = True

data Target = Personal -- affects only yourself
            | Area
            | Creature
    deriving (Eq, Ord, Generic, ToJSON, FromJSON, Enum, Show)

instance Fmt Target where
    fmt Personal = "Personal"
    fmt Area = "Area"
    fmt Creature = "Creature"

-- Spell set for prepared casters
type PrepSet = Map Spell Int

enumAll :: (Enum e) => [e]
enumAll = [toEnum 0 ..]


-- Defines a spell that can be cast
data Spell = Spell
    { castTime :: Text
    , components :: Set SpellComp
    , description :: Text
    , duration :: Text
    , range :: Text
    , savingThrow :: SavingThrow
    , spellLevel :: SpellLevelList
    , spellName :: Text
    , spellResist :: Bool
    , target :: Text
    }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, FromJSONKey, ToJSONKey)

data Inventory a = Inventory { goldPieces :: a
                             , inventoryItems :: [Text]
                             }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, FromJSONKey, ToJSONKey)

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
