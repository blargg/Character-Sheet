{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# language ApplicativeDo #-}
module Data.CharacterSheet where

import Data.Distributive
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Functor.Rep
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

instance Distributive Abilities where
    distribute abs = Abilities (fmap str abs)
                               (fmap dex abs)
                               (fmap con abs)
                               (fmap wis abs)
                               (fmap int abs)
                               (fmap cha abs)

data Ability = Strength
             | Dexterity
             | Constitution
             | Wisdom
             | Intelligence
             | Charisma
             deriving (Show)

-- TODO tests
-- Estabilish a connection between (Abilities a) and (Ability -> a)
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

abilityMod :: (Integral a, Num a) => a -> a
abilityMod score = (score - 10) `div` 2

data ClassData a = ClassData { level :: a
                             , bab :: a
                             , fortitude :: a
                             , reflex :: a
                             , will :: a
                             , classHealth :: a
                             }
                             deriving (Functor, Foldable, Traversable, Show)

chHealthA :: (Integral a, Num a, Applicative f) => Abilities (f a) -> ClassData (f a) -> (f a)
chHealthA abs cls = do
    con <-  (index abs Constitution)
    lvls <- level cls
    clsHP <- classHealth cls
    return $ (abilityMod con) * lvls + clsHP

chHealth :: (Integral a, Num a) => Abilities a -> ClassData a -> a
chHealth abs cls = runIdentity $ chHealthA (Identity <$> abs) (Identity <$> cls)


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
