{-# LANGUAGE OverloadedStrings #-}
module Frontend.Layout where

import Reflex.Dom.Core
import Reflex.Dom

grid :: (MonadWidget t m) => m a -> m a
grid = elClass "div" "grid"

row :: (MonadWidget t m) => m a -> m a
row = elClass "div" "row"

cell :: (MonadWidget t m) => m a -> m a
cell = elClass "div" "cell"
