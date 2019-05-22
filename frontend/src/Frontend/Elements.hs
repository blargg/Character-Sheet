{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Elements
    (urlLink)
        where

import Reflex.Dom
import Data.Text (Text)

urlLink :: DomBuilder t m => Text -> Text -> m ()
urlLink url displayText = elAttr "a" ("href" =: url) (text displayText)
