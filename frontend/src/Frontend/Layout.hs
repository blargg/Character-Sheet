{-# LANGUAGE OverloadedStrings #-}
module Frontend.Layout where

import Reflex.Dom.Core
import Reflex.Dom

import Data.Text (Text)
import Data.Text as T

grid :: (DomBuilder t m) => m a -> m a
grid = elClass "div" "grid"

row :: (DomBuilder t m) => m a -> m a
row = elClass "div" "row"

cell :: (DomBuilder t m) => m a -> m a
cell = elClass "div" "cell"

cellClass :: (DomBuilder t m) => Text -> m a -> m a
cellClass cl = elClass "div" ("cell " `mappend` cl)

labelCell :: (DomBuilder t m) => Text -> m ()
labelCell = cellClass "label" . text

statBlock :: (DomBuilder t m) => Text -> m a -> m a
statBlock title innerWidget = elClass "div" "statBlock" $ do
    elClass "h3" "blockHeader" $ text title
    innerWidget
