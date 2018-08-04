{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as M
import Text.Read (readMaybe)

import Common.Api
import Data.CharacterSheet
import Frontend.Input
import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (header, body)

header :: StaticWidget x ()
header = do
    el "title" $ text "Character Sheet"
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", static @"css/Style.css")]) $ return ()

body :: Widget x ()
body = do
  text "Character Sheet"
  el "p" $ text $ T.pack commonStuff
  val <- fmap (maybe 0 id) <$> numberInput
  text <- textInput def
  let character = invertId $ CharacterSheet val (value text)
  display character
  return ()

abilityDisplay :: (MonadWidget t m) => T.Text -> m (Dynamic t Int)
abilityDisplay name = undefined
    --abilityScore <- numberInput
