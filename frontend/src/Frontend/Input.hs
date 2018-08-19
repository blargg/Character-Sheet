{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Input where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup ((<>))
import Text.Read (readMaybe)

import Data.Functor.Misc
import GHCJS.DOM.Element
import Reflex.Dom.Core
import Reflex.Dom

import Common.Api
import Data.CharacterSheet

numberInput :: (Read a, MonadWidget t m) => m (Dynamic t (Maybe a))
numberInput = parseInput parse numberConfig
    where
        numberConfig = def & textInputConfig_inputType .~ "number"
                           & textInputConfig_attributes .~ pure (classAttr "numberInput number")
        parse :: (Read a) => Text -> Maybe a
        parse = readMaybe . T.unpack

type Attr = Map Text Text

-- Constructs Attr for a class string.
classAttr :: Text -> Attr
classAttr classes = "class" =: classes

-- TODO set a class depending on valid or invalid
parseInput :: MonadWidget t m => (T.Text -> Maybe a) -> TextInputConfig t -> m (Dynamic t (Maybe a))
parseInput parse config = do
    text <- textInput config
    return $ fmap parse $ value text

-- This is still experimental. The key event's aren't really sufficient to
-- detect when the span has changed in all cases.
editSpan :: MonadWidget t m => Text -> m (Dynamic t Text)
editSpan initialVal = do
    (spanElement, _) <- elAttr' "span" ("contenteditable" =: "true") $ text initialVal
    textUpdates initialVal spanElement

textUpdates :: (Reflex t, MonadWidget t m) => Text -> El t -> m (Dynamic t Text)
textUpdates initialVal element = do
    textUpdates <- performEvent $ (getInnerHTML (_element_raw element)) <$ select' Input
    holdDyn initialVal textUpdates
    where select' ev = select (_element_events element) (WrapArg ev)
