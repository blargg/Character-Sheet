{-# language DeriveGeneric  #-}
{-# language DeriveAnyClass  #-}
module Common.Api
    ( SpellSearch(..)
    , searchText
    ) where


import Data.Aeson
import Data.CharacterSheet
import Data.Text (Text)
import GHC.Generics


-- defines the search criteria when searching for spells
data SpellSearch = SpellSearch { prefix :: Text
                               , searchClass :: Maybe Class
                               }
    deriving (Generic, ToJSON, FromJSON)

searchText :: Text -> SpellSearch
searchText p = SpellSearch p Nothing
