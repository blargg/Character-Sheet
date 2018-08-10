{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend where

import Control.Monad (join)
import Data.Text (Text)
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
    skillMap <- skillsBlock pathfinderSkills
    el "p" $ display (sum <$> (skillRanks <$$> sequenceA skillMap))
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
armorBlock = mdo
    let addLines = attachWith (\m _ -> nextKey m) (current armorLines) addPressed
        removeLines = attachWithMaybe (\m x -> maxKey m) (current armorLines) removePressed
    armorLines <- addRemoveSet (0 =: ()) addLines removeLines
    armorVals <- grid $ do
        row $ ct "name" >> ct "ac"
        armorRows armorLines
    addPressed <- button "Add"
    removePressed <- button "Remove"
    return . join $ dynSum <$> armorVals
    where dynSum = foldl (\dx dy -> (+) <$> dx <*> dy) (pure 0)

nextKey :: M.Map Int a -> Int
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

skillsBlock :: (MonadWidget t m) => M.Map Text Ability -> m (M.Map Text (Dynamic t Skill))
skillsBlock statsConfig = grid $ M.traverseWithKey skillLine statsConfig

skillLine :: (MonadWidget t m) => Text -> Ability -> m (Dynamic t Skill)
skillLine skillName abl = row $ do
    ct skillName
    ct . shortName $ abl
    ranks <- fromMaybe 0 <$$> numberInput
    return $ Skill <$> pure skillName <*> pure False <*> pure abl <*> ranks <*> pure 0

ct :: (MonadWidget t m) => Text -> m ()
ct = cell . text
