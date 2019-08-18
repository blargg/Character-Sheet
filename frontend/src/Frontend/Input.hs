{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Input
    ( editSpan
    , numberInput
    ) where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Language.Javascript.JSaddle
import Reflex.Dom.Core

import Frontend.Javascript

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

-- A span that a user can edit in place (using contenteditable = true)
editSpan :: ( DomBuilder t m
            , MonadHold t m
            , PerformEvent t m
            , MonadJSM (Performable m)
            ) => Text -> Text -> m (Dynamic t Text)
editSpan elemId initialText = do
    (e, ()) <- elAttr' "span" editAttrs $ text initialText
    let changedEv = domEvent Input e
    let checkUpdate = fmap (fromMaybe "") . liftJSM $ innerTextById elemId
    let checkUpdateEv = checkUpdate <$ changedEv
    updatedValues <- performEvent checkUpdateEv
    holdDyn initialText updatedValues
    where editAttrs = "class" =: "single-line-edit"
                    <> "contenteditable" =: "true"
                    <> "id" =: elemId

innerTextById :: Text -> JSM (Maybe Text)
innerTextById elemId = do
    innerHTML <- jsg ("document" :: Text)
        ^. js1 ("getElementById" :: Text) elemId
        ^. js ("innerText" :: Text)
    jsvToText innerHTML
