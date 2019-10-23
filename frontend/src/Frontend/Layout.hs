{-# LANGUAGE OverloadedStrings #-}
module Frontend.Layout
    ( cell
    , cellClass
    , grid
    , labelCell
    , lbl'
    , row
    , space
    , space'
    , statBlock
    , statBlock'
    )where

import Reflex.Dom.Core

import Data.Text (Text)
import qualified Frontend.Bulma as Bulma
import qualified Frontend.Elements as E

grid :: (DomBuilder t m) => m a -> m a
grid = elClass "div" "grid"

row :: (DomBuilder t m) => m a -> m a
row = elClass "div" "trow"

cell :: (DomBuilder t m) => m a -> m a
cell = elClass "div" "cell"

cellClass :: (DomBuilder t m) => Text -> m a -> m a
cellClass cl = elClass "div" ("cell " `mappend` cl)

labelCell :: (DomBuilder t m) => Text -> m ()
labelCell = cellClass "lbl" . text

lbl' :: (DomBuilder t m) => Text -> m ()
lbl' = E.spanC "lbl" . text

statBlock :: (DomBuilder t m) => Text -> m a -> m a
statBlock title = statBlock' (text title) . const

statBlock' :: (DomBuilder t m) => m b -> (b -> m a) -> m a
statBlock' titleWidget innerWidget = elClass "div" "card statBlock" $ do
    headerValue <- Bulma.titleClass' "is-marginless" 4 $ titleWidget
    innerWidget headerValue

space :: (DomBuilder t m) => Text -> m ()
space width = E.spanAttr ("style" =: ("display:inline-block; width: " <> width)) $ pure ()

-- typical single space
space' :: (DomBuilder t m) => m ()
space' = space "0.5em"
