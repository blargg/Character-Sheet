{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Input
    ( Closed(..)
    , buttonC
    , editSpan
    , percentageInput
    , numberInput
    , numberInput'
    , expandCollapseButton
    ) where

import Common.Compose
import Control.Lens
import Control.Monad.Fix
import Data.CharacterSheet (Percentage(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Language.Javascript.JSaddle
import Reflex.Dom.Core
import Obelisk.Generated.Static

import Frontend.Javascript

-- Editable percentage. Allows for percentages over 100%
percentageInput :: (DomBuilder t m) => Percentage -> m (Dynamic t (Maybe Percentage))
percentageInput (Percentage x) = fmap Percentage <$$> numberInput x

numberInput :: ( Read a
               , Show a
               , DomBuilder t m
               )
               => a -> m (Dynamic t (Maybe a))
numberInput = numberInput' . Just

-- like number input, but allows empty initial value
numberInput' :: ( Read a
               , Show a
               , DomBuilder t m
               )
               => Maybe a -> m (Dynamic t (Maybe a))
numberInput' initialVal = parseInput parse numberConfig
    where
        numberConfig = def & elConf .~ (  classAttr "numberInput number"
                                       <> "type" =: "number")
                           & inputElementConfig_initialValue
                             .~ (T.pack . maybe "" show $ initialVal)
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

data Closed = Closed | Open
    deriving (Eq)

isOpen :: Bool -> Closed
isOpen True = Open
isOpen False = Closed

-- Creates a button that toggles, and changes image when clicked.
-- The appearance is suited for expand and collapse.
expandCollapseButton :: forall t m. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Closed -> m (Dynamic t Closed)
expandCollapseButton initState = mdo
    let initiallyOpen = initState == Open
    expState <- isOpen <$$> toggle initiallyOpen clickedEv :: m (Dynamic t Closed)
    (openSvgEl, _) <- mkSvg Open expState
    (closeSvgEl, _) <- mkSvg Closed expState
    let clickedEv :: Event t ()
        clickedEv = domEvent Click openSvgEl <> domEvent Click closeSvgEl
    return expState
    where
        mkSvg rep cur = elDynAttr'
            "img"
            (
                (displayStyle rep <$> cur)
                <> pure ("src" =: iconFor rep
                      <> "height" =: size
                      <> "width" =: size))
            (return ())
        iconFor Open = static @"icon/up_arrow.svg"
        iconFor Closed = static @"icon/down_arrow.svg"
        displayStyle rep cur = if rep == cur then mempty
                                             else "style" =: "display: none;"
        size = "10px"

buttonC :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonC className buttonText = do
    (e, ()) <- elClass' "button" className $ text buttonText
    return $ domEvent Click e
