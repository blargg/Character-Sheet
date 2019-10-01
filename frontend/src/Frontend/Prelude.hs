{-# LANGUAGE OverloadedStrings #-}
module Frontend.Prelude
    ( Attr
    , classAttr
    , module Common.Prelude
    ) where

import Data.Map (Map)
import Data.Text (Text)
import Reflex.Dom.Core
import Common.Prelude

type Attr = Map AttributeName Text

-- Constructs Attr for a class string.
classAttr :: Text -> Attr
classAttr classes = (AttributeName Nothing "class") =: classes
