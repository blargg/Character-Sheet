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
import Data.Dependent.Sum
import Data.Functor.Identity
import Obelisk.Backend
import Obelisk.Route
import Snap
import qualified Data.Aeson as Aeson

import Backend.Database as DB
import Common.Api

import Database.Persist.Sqlite

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = run_backend
  , _backend_routeEncoder = backendRouteEncoder
  }

run_backend :: ((R BackendRoute -> Snap ()) -> IO ()) -> IO ()
run_backend serve = do
    runSqlite "pf.db" $ do
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
          pages <- countPages search
          let curpage = page search
          jsonResponse
          writeLBS . Aeson.encode $ spellResponse sps curpage pages
      Nothing -> lift $ do
          modifyResponse $ setResponseStatus 500 "Internal Server Error"
          writeBS "500 error"
          finishWith =<< getResponse

jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setHeader "Content-Type" "application/json"
