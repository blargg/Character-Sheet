{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core
import Reflex.Dom
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Common.Api
import Common.Compose
import Data.CharacterSheet
import Frontend.Input
import Frontend.Layout
import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (header, body)

header :: StaticWidget x ()
header = do
    el "title" $ text "Character Sheet"
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", static @"css/Style.css")]) $ return ()
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", "https://fonts.googleapis.com/css?family=Roboto|Roboto+Mono")]) $ return ()

body :: Widget x ()
body = do
  text "Character Sheet"
  el "p" $ text $ T.pack commonStuff
  abs <- grid abilityBlock
  display (sequenceA abs)
  return ()

abilityBlock :: (MonadWidget t m) => m (Abilities (Dynamic t Int))
abilityBlock = Abilities <$> abilityDisplay "Str"
                         <*> abilityDisplay "Dex"
                         <*> abilityDisplay "Con"
                         <*> abilityDisplay "Wis"
                         <*> abilityDisplay "Int"
                         <*> abilityDisplay "Cha"

abilityDisplay :: (MonadWidget t m) => T.Text -> m (Dynamic t Int)
abilityDisplay name = row $ do
    cell $ text name
    abilityScore <- cell $ fromMaybe 10 <$$> numberInput
    cell $ display (abilityMod <$> abilityScore)
    return abilityScore
