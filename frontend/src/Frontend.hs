{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad (join)
import Control.Monad.Fix
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Reflex.Dom
import Reflex.Util

import Common.Route
import Common.Compose
import Data.CharacterSheet
import Frontend.Input
import Frontend.Layout

import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Obelisk.Frontend

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = header
  , _frontend_body = subRoute_ $ \x -> case x of
    FrontendRoute_Main -> sheet_body
    FrontendRoute_About -> about_page
  }

header :: (DomBuilder t m) => m ()
header = do
    el "title" $ text "Character Sheet"
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", static @"css/Style.css")]) $ return ()
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", "https://fonts.googleapis.com/css?family=Roboto|Roboto+Mono")]) $ return ()

about_page :: (DomBuilder t m
             , SetRoute t (R FrontendRoute) m
             , RouteToUrl (R FrontendRoute) m
              )
             => m ()
about_page = do
    el "div" $ routeLink (FrontendRoute_Main :/ ()) $ text "sheet"
    el "h1" $ text "About"
    el "p" $ text "GitHub: " >> urlLink projectURL "project page"
    el "h2" $ text "Caution"
    el "p" $ text "This website is experimental, and provided as is. \
        \This is not meant to hold onto a character sheet long term, \
        \and data might get lost over time. \
        \(As of writing, data is not saved after you leave the page.) \
        \Please copy down anything that you will need later."
    el "h2" $ text "Helping Out"
    el "p" $ do
        text "If you like this project, consider contributing "
        urlLink projectURL "code changes"
        text ", "
        urlLink issuesURL "suggestions"
        text ", or "
        urlLink issuesURL "bug reports"
        text " on the project page."
        where projectURL = "https://github.com/blargg/Character-Sheet"
              issuesURL = projectURL <> "/issues"

urlLink :: DomBuilder t m => Text -> Text -> m ()
urlLink url displayText = elAttr "a" ("href" =: url) (text displayText)

-- body :: ObeliskWidget t x route m => RoutedT t route m ()
sheet_body :: ( DomBuilder t m
              , PostBuild t m
              , MonadHold t m
              , MonadFix m
              , SetRoute t (R FrontendRoute) m
              , RouteToUrl (R FrontendRoute) m
              )
              => m ()
sheet_body = do
    el "div" $ routeLink (FrontendRoute_About :/ ()) $ text "about"
    el "h1" $ text "Character Sheet"
    rec abl <- flex $ do
            abl' <- abilityBlock
            healthBlock abl' cls
            combatManuverBlock abl' cls
            return abl'
        cls <- classBlock
    _ <- armorBlock -- armor class
    _ <- skillsBlock abl pathfinderSkills -- skill map
    return ()
    where flex = elClass "div" "flexContainer"

abilityBlock :: ( DomBuilder t m
                , PostBuild t m
                )
                => m (Abilities (Dynamic t Int))
abilityBlock = statBlock "Abilities" . grid $ do
    row $ lbl "Ability" >> lbl "Score" >> lbl "Mod"
    Abilities <$> abilityDisplay "Str"
              <*> abilityDisplay "Dex"
              <*> abilityDisplay "Con"
              <*> abilityDisplay "Wis"
              <*> abilityDisplay "Int"
              <*> abilityDisplay "Cha"

abilityDisplay :: ( DomBuilder t m
                  , PostBuild t m
                  ) => T.Text -> m (Dynamic t Int)
abilityDisplay name = row $ do
    cell $ text name
    abilityScore <- cell $ fromMaybe 10 <$$> numberInput
    cellClass "number" $ display (abilityMod <$> abilityScore)
    return abilityScore

classBlock :: (DomBuilder t m)
           => m (ClassData (Dynamic t Int))
classBlock = statBlock "Class" . grid $ do
    row $ lbl "Class Name" >> lbl "Level" >> lbl "HP" >> lbl "BAB" >> lbl "Fort" >> lbl "Ref" >> lbl "Will"
    row $ do
        _ <- cell $ inputElement def -- class name
        levels <- cell numDefZero
        hp <- cell numDefZero
        baseAttackBonus <- cell numDefZero
        fortSave <- cell numDefZero
        refSave <- cell numDefZero
        willSave <- cell numDefZero
        return $ ClassData levels baseAttackBonus fortSave refSave willSave hp
    where numDefZero = fromMaybe 0 <$$> numberInput

-- displays current total health, temp hp, wounds, remaining health
healthBlock :: ( DomBuilder t m
               , PostBuild t m
               ) => Abilities (Dynamic t Int) -> ClassData (Dynamic t Int) -> m ()
healthBlock abl cls = statBlock "Health" . grid $ do
    let hp = chHealthA abl cls
    row $ lbl "Max HP" >> lbl "Wounds" >> lbl "HP"
    row $ do
        cellNum (display hp)
        wnds <- cell $ fromMaybe 0 <$$> numberInput
        cellNum $ display ((-) <$> hp <*> wnds)
    where cellNum = cellClass "number"

combatManuverBlock :: ( DomBuilder t m
                      , PostBuild t m
                      )
                      => Abilities (Dynamic t Int) -> ClassData (Dynamic t Int) -> m ()
combatManuverBlock abl cls = statBlock "Combat Mnvr" . grid $ do
    row $ ct "CMB" >> cellNum (display cmb)
    row $ ct "CMD" >> cellNum (display cmd)
    where combatStats = do
              strengthMod <- str absMod
              dexterity <- dex absMod
              baseAttack <- bab cls
              return $ (strengthMod + baseAttack, strengthMod + dexterity + baseAttack + 10)
          cmb = fst <$> combatStats
          cmd = snd <$> combatStats
          absMod = abilityMod <$$> abl
          cellNum = cellClass "number"

armorBlock :: ( DomBuilder t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m
              )
              => m (Dynamic t Int)
armorBlock = statBlock "Armor" $ mdo
    let addLines = attachWith (\m _ -> nextKey m) (current armorLines) addPressed
        removeLines = removeEvents armorVals
    armorLines <- addRemoveSet (0 =: ()) addLines removeLines
    armorVals <- grid $ do
        row $ lbl "name" >> lbl "ac"
        armorRows armorLines
    addPressed <- el "div" $ button "Add"
    return . join $ dynSum <$> (fst <$$> armorVals)
    where dynSum = foldl (\dx dy -> (+) <$> dx <*> dy) (pure 0)
          removeEvents :: (Reflex t) => Dynamic t (M.Map k (b, Event t a)) -> (Event t a)
          removeEvents x = switchPromptlyDyn $ leftmost . fmap (snd . snd) . M.toList <$> x

nextKey :: (Num k) => M.Map k a -> k
nextKey = (1+) . fromMaybe 0 . maxKey

maxKey :: M.Map k a -> Maybe k
maxKey = fmap fst . lookupMax

lookupMax :: M.Map k a -> Maybe (k, a)
lookupMax m | M.null m = Nothing
            | otherwise = Just (M.findMax m)

armorRows :: ( DomBuilder t m
            , PostBuild t m
            , MonadHold t m
            , MonadFix m
            )
            => Dynamic t (M.Map Int ()) -> m (Dynamic t (M.Map Int (Dynamic t Int, Event t Int)))
armorRows ls = listWithKey ls keyedArmorRow

keyedArmorRow :: (DomBuilder t m)
              => Int -> a -> m (Dynamic t Int, Event t Int)
keyedArmorRow key  _ = do
          (v, ev) <- armorRow
          return (v, key <$ ev)

armorRow :: (DomBuilder t m)
            => m (Dynamic t Int, Event t ())
armorRow = row $ do
    _ <- cell $ inputElement def
    armorVal <- cell $ fromMaybe 0 <$$> numberInput
    delEvent <- button "delete line"
    return (armorVal, delEvent)

-- TODO: add some sort of filter to help find things quickly
skillsBlock :: ( DomBuilder t m
               , PostBuild t m
               )
               => Abilities (Dynamic t Int) -> M.Map Text Ability -> m (M.Map Text (Dynamic t Skill))
skillsBlock abl statsConfig = statBlock "Skills" . grid $ do
    row $ lbl "Skill" >> lbl "Ability" >> lbl "Class Skill" >> lbl "Ranks" >> lbl "misc. mod"
        >> lbl "total"
    M.traverseWithKey (skillLine abl) statsConfig

skillLine :: ( DomBuilder t m
             , PostBuild t m
             )
             => Abilities (Dynamic t Int) -> Text -> Ability -> m (Dynamic t Skill)
skillLine abls skillTitle abl = row $ do
    ct skillTitle
    ct . shortName $ abl
    classCB <- cell $ checkbox False def
    ranks <- cell $ fromMaybe 0 <$$> numberInput
    miscMod <- cell $ fromMaybe 0 <$$> numberInput
    let sk = Skill <$> pure skillTitle <*> value classCB <*> pure abl <*> ranks <*> miscMod
    cellClass "number" $ display (skillBonus <$> sequenceA abls <*> sk)
    return sk

ct :: (DomBuilder t m) => Text -> m ()
ct = cell . text

lbl :: (DomBuilder t m) => Text -> m ()
lbl = labelCell
