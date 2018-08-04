{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core
import Reflex.Dom
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Text.Read (readMaybe)

import Common.Api
import Data.CharacterSheet
import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (header, body)

header :: StaticWidget x ()
header = el "title" $ text "Character Sheet"

body :: Widget x ()
body = do
  text "Character Sheet"
  el "p" $ text $ T.pack commonStuff
  val <- fmap (maybe 0 id) <$> numberInput
  text <- textInput def
  let character = invert $ CharacterSheet val (value text)
  display character
  return ()


-- TODO set a class depending on valid or invalid
numberInput :: MonadWidget t m => m (Dynamic t (Maybe Int))
numberInput = do
    text <- textInput numberConfig
    return $ fmap parse $ value text
    where
        numberConfig = def & textInputConfig_inputType .~ "number"
        parse :: T.Text -> Maybe Int
        parse = readMaybe . T.unpack
