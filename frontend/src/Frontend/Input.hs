{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Input
    ( numberInput
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens
import Data.Map (Map)
import Text.Read (readMaybe)

import Reflex.Dom.Core

numberInput :: ( Read a
               , Show a
               , DomBuilder t m
               )
               => a -> m (Dynamic t (Maybe a))
numberInput initialVal = parseInput parse numberConfig
    where
        numberConfig = def & elConf .~ (  classAttr "numberInput number"
                                       <> "type" =: "number")
                           & inputElementConfig_initialValue
                             .~ (T.pack . show $ initialVal)
        elConf = inputElementConfig_elementConfig . elementConfig_initialAttributes
        parse :: (Read a) => Text -> Maybe a
        parse = readMaybe . T.unpack

type Attr = Map AttributeName Text

-- Constructs Attr for a class string.
classAttr :: Text -> Attr
classAttr classes = (AttributeName Nothing "class") =: classes

-- TODO set a class depending on valid or invalid
parseInput :: (DomBuilder t m)
           => (T.Text -> Maybe a) -> InputElementConfig er t (DomBuilderSpace m) -> m (Dynamic t (Maybe a))
parseInput parse config = do
    textValue <- inputElement config
    return $ fmap parse $ value textValue
