{-# language DeriveGeneric  #-}
{-# language DeriveAnyClass  #-}
module Common.Api
    ( PagedResponse(..)
    , PagedSearch(..)
    , SpellQuery(..)
    , SpellSearch
    , SpellSearchResponse
    , spellSearch
    , spellResponse
    ) where


import Data.Aeson
import Data.CharacterSheet
import Data.Text (Text)
import GHC.Generics

-- search for a specific page out of a query
data PagedSearch a = PagedSearch { query :: a
                                 , page :: Int
                                 }
    deriving (Generic, ToJSON, FromJSON)

data PagedResponse a = PagedResponse { pageData :: a
                                     , currentPage :: Int
                                     , totalPages :: Int
                                     }
    deriving (Generic, ToJSON, FromJSON)

type SpellSearch = PagedSearch SpellQuery

spellSearch :: Text -> Maybe Class -> Maybe SpellLevel -> Maybe SpellLevel -> Int -> SpellSearch
spellSearch p cl minL maxL = PagedSearch (SpellQuery p cl minL maxL)

-- defines the search criteria when searching for spells
data SpellQuery = SpellQuery { prefix :: Text
                               , searchClass :: Maybe Class
                               , minLevel :: Maybe SpellLevel
                               , maxLevel :: Maybe SpellLevel
                               }
    deriving (Generic, ToJSON, FromJSON)

type SpellSearchResponse = PagedResponse [Spell]

spellResponse :: [Spell] -> Int -> Int -> SpellSearchResponse
spellResponse = PagedResponse
