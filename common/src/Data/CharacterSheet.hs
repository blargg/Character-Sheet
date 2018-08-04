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

invert :: (Applicative f) => CharacterSheet f -> f (CharacterSheet Identity)
invert (CharacterSheet nu na) = CharacterSheet <$> fmap Identity nu <*> fmap Identity na
