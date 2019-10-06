{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Prelude
    ( AppWidget
    , Attr
    , classAttr
    , module Common.Prelude
    , module Control.Monad.Fix
    ) where

import Data.Map (Map)
import Data.Text (Text)
import Reflex.Dom.Core
import Common.Prelude
import Control.Monad.Fix
import Language.Javascript.JSaddle

type Attr = Map AttributeName Text

-- Constructs Attr for a class string.
classAttr :: Text -> Attr
classAttr classes = (AttributeName Nothing "class") =: classes

-- context of an app widget
-- This runs in a prerendered context
type AppWidget t m =
    ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , TriggerEvent t m
    , PostBuild t m
    , PerformEvent t m
    , MonadJSM m
    , MonadJSM (Performable m)
    )

