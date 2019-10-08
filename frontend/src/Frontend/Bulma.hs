{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Bulma
    ( textInput
    , title
    , subtitle
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Frontend.Prelude
import Reflex.Dom.Core hiding (textInput)

-- basic definition of a materialize styled text input
textInput :: (DomBuilder t m) => Text -> m (Dynamic t Text)
textInput placeholder = do
    let config = def & elConf .~ (classAttr "input is-rounded"
                                 <> "type" =: "text"
                                 <> "placeholder" =: placeholder
                                 )
    textValue <- inputElement config
    return (value textValue)
        where
            elConf = inputElementConfig_elementConfig . elementConfig_initialAttributes

title :: (DomBuilder t m) => Int -> Text -> m ()
title level s = elClass ("h" <> l) ("title is-" <> l) $ text s
    where l = Text.pack (show level)

subtitle :: (DomBuilder t m) => Int -> Text -> m ()
subtitle level s = elClass ("h" <> l) ("subtitle is-" <> l) $ text s
    where l = Text.pack (show level)
