{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad (join)
import Data.Functor.Rep
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Semigroup ((<>))

import Data.Functor.Misc
import Reflex.Dom
import Reflex.Dom.Core

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
    skillMap <- skillsBlock abs pathfinderSkills
    el "p" $ display $ do
        abs' <- sequenceA abs
        skM <- sequenceA skillMap
        return $ sum (fmap (skillBonus abs') skM)
    return ()

abilityBlock :: (MonadWidget t m) => m (Abilities (Dynamic t Int))
abilityBlock = do
    row $ lbl "Ability" >> lbl "Score" >> lbl "Mod"
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
    row $ lbl "Class Name" >> lbl "Level" >> lbl "BAB" >> lbl "Fort" >> lbl "Ref" >> lbl "Will"
    row $ do
        className <- cell $ textInput def
        levels <- cell numDefZero
        baseAttackBonus <- cell numDefZero
        fortSave <- cell numDefZero
        refSave <- cell numDefZero
        willSave <- cell numDefZero
        return $ ClassData levels baseAttackBonus fortSave refSave willSave
    where numDefZero = fromMaybe 0 <$$> numberInput

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
armorBlock = mdo
    let addLines = attachWith (\m _ -> nextKey m) (current armorLines) addPressed
        removeLines = attachWithMaybe (\m x -> maxKey m) (current armorLines) removePressed
    armorLines <- addRemoveSet (0 =: ()) addLines removeLines
    armorVals <- grid $ do
        row $ lbl "name" >> lbl "ac"
        armorRows armorLines
    addPressed <- button "Add"
    removePressed <- button "Remove"
    return . join $ dynSum <$> armorVals
    where dynSum = foldl (\dx dy -> (+) <$> dx <*> dy) (pure 0)

nextKey :: (Ord k, Num k) => M.Map k a -> k
nextKey = (1+) . fromMaybe 0 . maxKey

maxKey :: M.Map k a -> Maybe k
maxKey = fmap fst . lookupMax

lookupMax :: M.Map k a -> Maybe (k, a)
lookupMax m | M.null m = Nothing
            | otherwise = Just (M.findMax m)

-- TODO: move to a module
-- TODO: may want a version that is specific to adding/removing the end of the
-- set
addRemoveSet :: (MonadWidget t m, Ord k) => M.Map k () -> Event t k -> Event t k -> m (Dynamic t (M.Map k ()))
addRemoveSet initSet addItem removeItem = foldDyn addOrRemove initSet addRemoveE
    where addRemoveE = (Right <$> addItem) <> (Left <$> removeItem)
          addOrRemove (Right k) m = M.insert k () m
          addOrRemove (Left k) m = M.delete k m

armorRows :: (MonadWidget t m) => Dynamic t (M.Map Int ()) -> m (Dynamic t (M.Map Int (Dynamic t Int)))
armorRows lines = list lines (const armorRow)

armorRow :: (MonadWidget t m) => m (Dynamic t Int)
armorRow = row $ do
    cell $ textInput def
    cell $ fromMaybe 0 <$$> numberInput

-- TODO: add some sort of filter to help find things quickly
skillsBlock :: (MonadWidget t m) => Abilities (Dynamic t Int) -> M.Map Text Ability -> m (M.Map Text (Dynamic t Skill))
skillsBlock abl statsConfig = grid $ do
    row $ lbl "Skill" >> lbl "Ability" >> lbl "Class Skill" >> lbl "Ranks" >> lbl "misc. mod"
        >> lbl "total"
    M.traverseWithKey (skillLine abl) statsConfig

skillLine :: (MonadWidget t m) => Abilities (Dynamic t Int) -> Text -> Ability -> m (Dynamic t Skill)
skillLine abls skillName abl = row $ do
    ct skillName
    ct . shortName $ abl
    classCB <- cell $ checkbox False def
    ranks <- cell $ fromMaybe 0 <$$> numberInput
    miscMod <- cell $ fromMaybe 0 <$$> numberInput
    let sk = Skill <$> pure skillName <*> value classCB <*> pure abl <*> ranks <*> miscMod
    cell $ display (skillBonus <$> sequenceA abls <*> sk)
    return sk

ct :: (MonadWidget t m) => Text -> m ()
ct = cell . text

lbl :: (MonadWidget t m) => Text -> m ()
lbl = labelCell
