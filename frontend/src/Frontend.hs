{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad (join)
import qualified Data.Text as T
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Semigroup ((<>))

import Reflex.Dom.Core
import Reflex.Dom

import Common.Api
import Common.Compose
import Data.CharacterSheet
import Frontend.Input
import Frontend.Layout
import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (header, body)

header :: StaticWidget x ()
header = do
    el "title" $ text "Character Sheet"
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", static @"css/Style.css")]) $ return ()
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", "https://fonts.googleapis.com/css?family=Roboto|Roboto+Mono")]) $ return ()

body :: Widget x ()
body = do
    el "h1" $ text "Character Sheet"
    abs <- grid abilityBlock
    display (sequenceA abs)
    cls <- classBlock
    el "p" $ display (sequenceA cls)
    combatManuverBlock abs cls
    armorClass <- armorBlock
    el "p" $ display armorClass
    return ()

abilityBlock :: (MonadWidget t m) => m (Abilities (Dynamic t Int))
abilityBlock = do
    row $ cell (text "Ability") >> cell (text "Score") >> cell (text "Mod")
    Abilities <$> abilityDisplay "Str"
              <*> abilityDisplay "Dex"
              <*> abilityDisplay "Con"
              <*> abilityDisplay "Wis"
              <*> abilityDisplay "Int"
              <*> abilityDisplay "Cha"

abilityDisplay :: (MonadWidget t m) => T.Text -> m (Dynamic t Int)
abilityDisplay name = row $ do
    cell $ text name
    abilityScore <- cell $ fromMaybe 10 <$$> numberInput
    cellClass "number" $ display (abilityMod <$> abilityScore)
    return abilityScore

classBlock :: (MonadWidget t m) => m (ClassData (Dynamic t Int))
classBlock = grid $ do
    row $ ct "Class Name" >> ct "Level" >> ct "BAB" >> ct "Fort" >> ct "Ref" >> ct "Will"
    row $ do
        className <- cell $ textInput def
        levels <- cell numDefZero
        baseAttackBonus <- cell numDefZero
        fortSave <- cell numDefZero
        refSave <- cell numDefZero
        willSave <- cell numDefZero
        return $ ClassData levels baseAttackBonus fortSave refSave willSave
    where ct = cell . text
          numDefZero = fromMaybe 0 <$$> numberInput

combatManuverBlock :: (MonadWidget t m) => Abilities (Dynamic t Int) -> ClassData (Dynamic t Int) -> m ()
combatManuverBlock abs cls = grid $ do
    row $ ct "CMB" >> cell (display cmb)
    row $ ct "CMD" >> cell (display cmd)
    where combatStats = do
              strengthMod <- str absMod
              dexterity <- dex absMod
              baseAttack <- bab cls
              return $ (strengthMod + baseAttack, strengthMod + dexterity + baseAttack + 10)
          cmb = fst <$> combatStats
          cmd = snd <$> combatStats
          absMod = abilityMod <$$> abs

armorBlock :: (MonadWidget t m) => m (Dynamic t Int)
armorBlock = do
    armorVals <- grid $ do
        row $ ct "name" >> ct "ac"
        armorRows (pure $ 1 =: () <> 2 =: ())
    addPressed <- button "Add Row"
    removePressed <- button "Remove Row"
    return . join $ dynSum <$> armorVals
    where dynSum = foldl (\dx dy -> (+) <$> dx <*> dy) (pure 0)

dSet :: Dynamic t (M.Map k ())
dSet = undefined

armorRows :: (MonadWidget t m) => Dynamic t (M.Map Int ()) -> m (Dynamic t (M.Map Int (Dynamic t Int)))
armorRows lines = list lines (const armorRow)

armorRow :: (MonadWidget t m) => m (Dynamic t Int)
armorRow = row $ do
    cell $ textInput def
    cell $ fromMaybe 0 <$$> numberInput

ct :: (MonadWidget t m) => T.Text -> m ()
ct = cell . text
