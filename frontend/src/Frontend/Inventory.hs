{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Inventory (main) where

import Reflex.Dom

import Data.CharacterSheet
import Data.Text (Text)
import Frontend.Prelude
import Frontend.Layout
import Frontend.Input
import qualified Frontend.Elements as E
import Frontend.Storage hiding (StorageKey(..))
import qualified Frontend.Storage as K

main :: AppWidget t m => m ()
main = do
    _ <- stashValue K.Inventory inventory
    return ()

inventory :: AppWidget t m => Maybe (Inventory Int) -> m (Dynamic t (Inventory Int))
inventory minit = do
    gp <- money (fmap goldPieces minit)
    itms <- items (fmap inventoryItems minit)
    return $ Inventory <$> gp <*> itms

longNumberConfig :: a -> NumberConfig a
longNumberConfig x = NumberConfig (Just x) "num"

money :: AppWidget t m => Maybe Int -> m (Dynamic t Int)
money minit = statBlock "Money" $ do
    let startingGold = fromMaybe 0 minit
    let config = longNumberConfig startingGold
    gp <- fmap (fromMaybe 0) <$> numberInput' config
    space "0.5em"
    E.span $ text "gp"
    return gp

items :: AppWidget t m => Maybe [Text] -> m (Dynamic t [Text])
items minit = statBlock "Items" $ do
    let initialList = fromMaybe [""] minit
    join <$> listWidget initialList "" itemLine'

itemLine' :: AppWidget t m => Text -> m (Dynamic t [Text])
itemLine' initT = (fmap . fmap) (:[]) $ textSimple initT
