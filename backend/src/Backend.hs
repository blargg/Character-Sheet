{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Data.CharacterSheet
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Obelisk.Backend
import Obelisk.Route
import Snap
import qualified Data.Aeson as Aeson

import Common.Api
import Common.Prelude

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve server
  , _backend_routeEncoder = backendRouteEncoder
  }

server :: R BackendRoute -> Snap ()
server (BackendRoute_Missing :=> Identity ()) = writeBS "missing"
server (BackendRoute_API :=> Identity _) = dir "api" $
    route [ ("echo/:echoparam", echoHandler)
          , ("spelllist/", spellListHandler)
          ]

echoHandler :: Snap ()
-- echoHandler = writeBS . rqContextPath =<< getRequest
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL") writeBS param

spellListHandler :: Snap ()
spellListHandler = do
    rb <- readRequestBody 2048
    let m_search = Aeson.decode rb :: Maybe SpellSearch
    writeLBS . Aeson.encode $ searchSpells m_search

searchSpells :: Maybe SpellSearch -> [Spell]
searchSpells Nothing = exampleSpells
searchSpells (Just SpellSearch{..}) =
    let pre = fromMaybe "" prefix
     in filter (spellFilter pre) exampleSpells

spellFilter :: Text -> Spell -> Bool
spellFilter search sp = Text.isPrefixOf search (spellName sp)

-- example list of spells for initial testing
exampleSpells :: [Spell]
exampleSpells = [ fireball, ray_of_frost]

fireball :: Spell
fireball = Spell { spellName = "fireball"
                 , spellLevel = SpellLevelList $ Wizard =: SpellLevel 3
                 , description = "shoots fireball at person"
                 , components = Set.fromList [ Verbal, Somantic, Material ]
                 , castTime = StandardAction
                 , duration = "instanteneous"
                 , range = "400ft + 40ft / level"
                 , savingThrow = Ref
                 , spellResist = True
                 , target = Area
                 }

ray_of_frost :: Spell
ray_of_frost = Spell { spellName = "ray of frost"
                 , spellLevel = SpellLevelList $ Wizard =: SpellLevel 0
                 , description = "beam of frost, slows"
                 , components = Set.fromList [ Verbal, Somantic ]
                 , castTime = StandardAction
                 , duration = "instanteneous"
                 , range = "25ft + 5ft / 2 level"
                 , savingThrow = None
                 , spellResist = True
                 , target = Creatures 1
                 }
