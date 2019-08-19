{-# language DeriveGeneric  #-}
{-# language DeriveAnyClass  #-}
module Common.Api
    ( SpellSearch(..)
    , searchText
    ) where


import Data.Aeson
import Data.Text (Text)
import GHC.Generics


-- defines the search criteria when searching for spells
data SpellSearch = SpellSearch { prefix :: Maybe Text
                               }
    deriving (Generic, ToJSON, FromJSON)

searchText :: Text -> SpellSearch
searchText p = SpellSearch (Just p)
