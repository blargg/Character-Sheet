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
    , CharacterSheet(..)
    , Class(..)
    , ClassData(..)
    , Dice(..)
    , Die
    , Fmt(..)
    , GameDuration(..)
    , Named(..)
    , Nmd
    , Percentage(..)
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
    , d -- single die
    , highestFaceValue
    , initiative'
    , nmd
    , pathfinderSkills
    , fmtComps
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
import Database.Esqueleto
import GHC.Generics

-- Class for things that have a text representation that can be shown to the
-- user
class Fmt a where
    fmt :: a -> Text

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

data ClassData a = ClassData { className :: Text
                             , level :: a
                             , bab :: a
                             , fortitude :: a
                             , reflex :: a
                             , will :: a
                             , classHealth :: a
                             }
                             deriving (Generic, ToJSON, FromJSON, Functor, Foldable, Traversable, Read, Show)

blankClass :: ClassData Int
blankClass = ClassData { className = ""
                       , level = 0
                       , bab = 0
                       , fortitude = 0
                       , reflex = 0
                       , will = 0
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
data SpellComp = Verbal | Somantic | Material | Focus | DevineFocus
    deriving (Eq, Ord, Generic, ToJSON, FromJSON, Enum, Show)

instance Fmt SpellComp where
    fmt Verbal = "Verbal"
    fmt Somantic = "Somantic"
    fmt Material = "Material"
    fmt Focus = "Focus"
    fmt DevineFocus = "DevineFocus"

fmtComps :: Set SpellComp -> Text
fmtComps scs = mconcat $ List.intersperse ", " $ fmap fmt $ Set.toList scs

data GameDuration = FreeAction
                  | StandardAction
                  | FullRound
    deriving (Generic, ToJSON, FromJSON, Enum, Show)

instance Fmt GameDuration where
    fmt FreeAction = "Free Action"
    fmt StandardAction = "Standard Action"
    fmt FullRound = "Full Round"

data SavingThrow = Fort | Ref | Will | None
    deriving (Generic, ToJSON, FromJSON, Enum, Show)

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
    deriving (Eq, Generic, ToJSON, FromJSON)

data SpellLevelList = SpellLevelList (Map Class SpellLevel)
    deriving (Generic, ToJSON, FromJSON)

data Class = Bard
           | Barbarian
           | Cleric
           | Druid
           | Fighter
           | Monk
           | Paladin
           | Ranger
           | Rogue
           | Sorcerer
           | Wizard
           | Other Text
           deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data Target = Personal -- affects only yourself
            | Area
            | Creature
    deriving (Generic, ToJSON, FromJSON, Enum, Show)

instance Fmt Target where
    fmt Personal = "Personal"
    fmt Area = "Area"
    fmt Creature = "Creature"

-- Defines a spell that can be cast
data Spell = Spell
    { castTime :: GameDuration
    , components :: Set SpellComp
    , description :: Text
    , duration :: Text
    , range :: Text
    , savingThrow :: SavingThrow
    , spellLevel :: SpellLevelList
    , spellName :: Text
    , spellResist :: Bool
    , target :: Target
    }
    deriving (Generic, ToJSON, FromJSON)

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

-- ### Database feilds ###
instance PersistField SavingThrow where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 x) = Right . toEnum . fromIntegral $ x
    fromPersistValue _ = Left "PersistField.SavingThrow.fromPersistValue called on wrong data type"

instance PersistFieldSql SavingThrow where
    sqlType _ = SqlInt32

instance PersistField SpellComp where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 x) = Right . toEnum . fromIntegral $ x
    fromPersistValue _ = Left "PersistField.SavingThrow.fromPersistValue called on wrong data type"

instance PersistFieldSql SpellComp where
    sqlType _ = SqlInt32

instance PersistField GameDuration where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 x) = Right . toEnum . fromIntegral $ x
    fromPersistValue _ = Left "PersistField.SavingThrow.fromPersistValue called on wrong data type"

instance PersistFieldSql GameDuration where
    sqlType _ = SqlInt32

instance PersistField Target where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 x) = Right . toEnum . fromIntegral $ x
    fromPersistValue _ = Left "PersistField.SavingThrow.fromPersistValue called on wrong data type"

instance PersistFieldSql Target where
    sqlType _ = SqlInt32
