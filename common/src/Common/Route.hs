{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Route
    ( BackendRoute(..)
    , FrontendRoute(..)
    , backendRouteEncoder
    , pageName
    , samePage
    )where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Control.Category as Cat
import Data.Text (Text)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_API :: BackendRoute PageName
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_About :: FrontendRoute ()
  FrontendRoute_License :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

-- Name of the page to display to the user
pageName :: FrontendRoute x -> Text
pageName FrontendRoute_Main = "Character Sheet"
pageName FrontendRoute_About = "About"
pageName FrontendRoute_License = "License"

samePage :: FrontendRoute a -> FrontendRoute b -> Bool
samePage FrontendRoute_Main FrontendRoute_Main = True
samePage FrontendRoute_About FrontendRoute_About = True
samePage FrontendRoute_License FrontendRoute_License = True
samePage _ _ = False

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
backendRouteEncoder = mkFullRouteEncoder
    (FullRoute_Backend BackendRoute_Missing :/ ())
    (\case
      BackendRoute_API -> PathSegment "api" $ Cat.id
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    )
    (\case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_About -> PathSegment "about" $ unitEncoder mempty
      FrontendRoute_License -> PathSegment "license" $ unitEncoder mempty
    )


concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
