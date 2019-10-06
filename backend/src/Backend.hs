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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.CharacterSheet
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Obelisk.Backend
import Obelisk.Route
import Snap
import qualified Data.Aeson as Aeson

import Backend.Database as DB
import Common.Api
import Common.Prelude

import Database.Persist.Sqlite

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = run_backend
  , _backend_routeEncoder = backendRouteEncoder
  }

run_backend :: ((R BackendRoute -> Snap ()) -> IO ()) -> IO ()
run_backend serve = do
    runSqlite ":memory:" $ do
        createDatabase exampleSpells
        transactionSave
        sql <- ask
        liftIO $ serve (server sql)

server :: SqlBackend -> R BackendRoute -> Snap ()
server _ (BackendRoute_Missing :=> Identity ()) = writeBS "missing"
server sql (BackendRoute_API :=> Identity _) = dir "api" $
    route [ ("echo/:echoparam", echoHandler)
          , ("spelllist/", runReaderT spellListHandler sql)
          ]

echoHandler :: Snap ()
-- echoHandler = writeBS . rqContextPath =<< getRequest
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL") writeBS param

spellListHandler :: ReaderT SqlBackend Snap ()
spellListHandler = do
    rb <- readRequestBody 2048
    case Aeson.decode rb :: Maybe SpellSearch of
      Just search -> do
          sps <- searchSpells search
          liftIO . putStrLn $ "spells retrieved: " ++ show (length sps)
          writeLBS . Aeson.encode $ sps
      Nothing -> lift $ do
          modifyResponse $ setResponseStatus 500 "Internal Server Error"
          writeBS "500 error"
          finishWith =<< getResponse

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
                 , target = Creature
                 }
