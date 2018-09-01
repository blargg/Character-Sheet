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
    healthBlock abs cls
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
    row $ lbl "Class Name" >> lbl "Level" >> lbl "HP" >> lbl "BAB" >> lbl "Fort" >> lbl "Ref" >> lbl "Will"
    row $ do
        className <- cell $ textInput def
        levels <- cell numDefZero
        hp <- cell numDefZero
        baseAttackBonus <- cell numDefZero
        fortSave <- cell numDefZero
        refSave <- cell numDefZero
        willSave <- cell numDefZero
        return $ ClassData levels baseAttackBonus fortSave refSave willSave hp
    where numDefZero = fromMaybe 0 <$$> numberInput

-- displays current total health, temp hp, wounds, remaining health
healthBlock :: (MonadWidget t m) => Abilities (Dynamic t Int) -> ClassData (Dynamic t Int) -> m ()
healthBlock abs cls = grid $ do
    let hp = chHealthA abs cls
    row $ lbl "Max HP" >> lbl "Wounds" >> lbl "HP"
    row $ do
        cellNum (display hp)
        wnds <- cell $ fromMaybe 0 <$$> numberInput
        cellNum $ display ((-) <$> hp <*> wnds)
    where cellNum = cellClass "number"

combatManuverBlock :: (MonadWidget t m) => Abilities (Dynamic t Int) -> ClassData (Dynamic t Int) -> m ()
combatManuverBlock abs cls = grid $ do
    row $ ct "CMB" >> cellNum (display cmb)
    row $ ct "CMD" >> cellNum (display cmd)
    where combatStats = do
              strengthMod <- str absMod
              dexterity <- dex absMod
              baseAttack <- bab cls
              return $ (strengthMod + baseAttack, strengthMod + dexterity + baseAttack + 10)
          cmb = fst <$> combatStats
          cmd = snd <$> combatStats
          absMod = abilityMod <$$> abs
          cellNum = cellClass "number"

armorBlock :: (MonadWidget t m) => m (Dynamic t Int)
armorBlock = mdo
    let addLines = attachWith (\m _ -> nextKey m) (current armorLines) addPressed
        removeLines = removeEvents armorVals
    armorLines <- addRemoveSet (0 =: ()) addLines removeLines
    armorVals <- grid $ do
        row $ lbl "name" >> lbl "ac"
        armorRows armorLines
    addPressed <- button "Add"
    return . join $ dynSum <$> (fst <$$> armorVals)
    where dynSum = foldl (\dx dy -> (+) <$> dx <*> dy) (pure 0)
          removeEvents :: (Reflex t) => Dynamic t (M.Map k (b, Event t a)) -> (Event t a)
          removeEvents x = switchPromptlyDyn $ leftmost . fmap (snd . snd) . M.toList <$> x

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

armorRows :: (MonadWidget t m) => Dynamic t (M.Map Int ()) -> m (Dynamic t (M.Map Int ((Dynamic t Int), Event t Int)))
armorRows lines = listWithKey lines keyedArmorRow

keyedArmorRow :: (MonadWidget t m) => Int -> a -> m (Dynamic t Int, Event t Int)
keyedArmorRow key  _ = do
          (v, ev) <- armorRow
          return (v, key <$ ev)

armorRow :: (MonadWidget t m) => m (Dynamic t Int, Event t ())
armorRow = row $ do
    cell $ textInput def
    armorVal <- cell $ fromMaybe 0 <$$> numberInput
    delEvent <- button "delete line"
    return (armorVal, delEvent)

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
    cellClass "number" $ display (skillBonus <$> sequenceA abls <*> sk)
    return sk

ct :: (MonadWidget t m) => Text -> m ()
ct = cell . text

lbl :: (MonadWidget t m) => Text -> m ()
lbl = labelCell
