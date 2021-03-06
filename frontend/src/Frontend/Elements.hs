{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Elements
    ( div
    , divC
    , span
    , spanAttr
    , spanC
    , urlLink
    )
        where

import Prelude hiding (div, span)
import Reflex.Dom

import Data.Map (Map)
import Data.Text (Text)

-- | This module is meant to be imported qualified, many of the names are short,
-- and conflict with common operations.
--
-- el "tag" -> tag
-- elAttr "tag" -> tagA
-- elClass "tag" -> tagC

urlLink :: DomBuilder t m => Text -> Text -> m ()
urlLink url displayText = elAttr "a" ("href" =: url) (text displayText)

div :: DomBuilder t m => m a -> m a
div = el "div"

divC :: DomBuilder t m => Text -> m a -> m a
divC = elClass "div"

span :: DomBuilder t m => m a -> m a
span = el "span"

spanC :: DomBuilder t m => Text -> m a -> m a
spanC = elClass "span"

spanAttr :: DomBuilder t m => Map Text Text -> m a -> m a
spanAttr = elAttr "span"
