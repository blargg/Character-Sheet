{-# LANGUAGE OverloadedStrings #-}
module Frontend.Layout where

import Reflex.Dom.Core
import Reflex.Dom

import Data.Text (Text)
import Data.Text as T

grid :: (MonadWidget t m) => m a -> m a
grid = elClass "div" "grid"

row :: (MonadWidget t m) => m a -> m a
row = elClass "div" "row"

cell :: (MonadWidget t m) => m a -> m a
cell = elClass "div" "cell"

cellClass :: (MonadWidget t m) => Text -> m a -> m a
cellClass cl = elClass "div" ("cell " `mappend` cl)

labelCell :: (MonadWidget t m) => Text -> m ()
labelCell = cellClass "label" . text

statBlock :: (MonadWidget t m) => Text -> m a -> m a
statBlock title innerWidget = elClass "div" "statBlock" $ do
    elClass "h3" "blockHeader" $ text title
    innerWidget
