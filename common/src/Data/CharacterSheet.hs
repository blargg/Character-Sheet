{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
module Data.CharacterSheet where

import Data.Functor.Classes
import Data.Functor.Identity
import qualified Data.Text as T

data CharacterSheet f = CharacterSheet { num :: f Int
                                       , name :: f T.Text
                                       }

instance Show1 f => Show (CharacterSheet f) where
    show (CharacterSheet nu na) = showString "CharacterSheet "
                                . showsPrec1 0 nu
                                . showString " "
                                . showsPrec1 0 na
                                $ ""

invert :: (Applicative f, Applicative g) => CharacterSheet f -> f (CharacterSheet g)
invert (CharacterSheet nu na) = CharacterSheet <$> fmap pure nu <*> fmap pure na

invertId :: (Applicative f) => CharacterSheet f -> f (CharacterSheet Identity)
invertId = invert

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
