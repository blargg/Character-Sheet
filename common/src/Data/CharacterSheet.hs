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
    , CharacterSheet(..)
    , ClassData(..)
    , Percentage(..)
    , Named(..)
    , Nmd
    , Skill(..)
    , abilityMod
    , blankArmor
    , blankClass
    , blankSkill
    , chHealth
    , nmd
    , pathfinderSkills
    , shortName
    , showPercentage
    , skillBonus
    ) where

import Data.Aeson
import Data.Distributive
import Data.Functor.Identity
import Data.Functor.Rep
import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

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
