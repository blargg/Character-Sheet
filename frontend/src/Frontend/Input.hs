{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Input where

import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core
import Reflex.Dom
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Text.Read (readMaybe)

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
