module Reflex.Util where

import Reflex

import Control.Monad.Fix
import qualified Data.Map as M
import Data.Semigroup ((<>))

addRemoveSet :: ( Reflex t
                , MonadFix m
                , MonadHold t m
                , Ord k
                ) =>
                    M.Map k () -> Event t k -> Event t k -> m (Dynamic t (M.Map k ()))
addRemoveSet initSet addItem removeItem = foldDyn addOrRemove initSet addRemoveE
    where addRemoveE = (Right <$> addItem) <> (Left <$> removeItem)
          addOrRemove (Right k) m = M.insert k () m
          addOrRemove (Left k) m = M.delete k m

