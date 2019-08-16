{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend (frontend) where

import Control.Monad (join)
import Control.Monad.Fix
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Reflex.Dom

import Common.Route
import Common.Compose
import Data.CharacterSheet
import Frontend.Input
import Frontend.Layout
import Frontend.Storage hiding (StorageKey(..))
import qualified Frontend.Storage as K
import qualified Frontend.About as About
import qualified Frontend.Elements as E

import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Obelisk.Frontend

import Language.Javascript.JSaddle

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = header
  , _frontend_body = body
  }

body :: ( DomBuilder t m
        , MonadHold t m
        , MonadFix m
        , SetRoute t (R FrontendRoute) m
        , RouteToUrl (R FrontendRoute) m
        , Prerender js t m
        )
     => RoutedT t (R FrontendRoute) m ()
body = subRoute_ $ \x -> do
    navigation x $ case x of
        FrontendRoute_Main -> prerender_ (text "loading") sheet_body
        FrontendRoute_About -> About.main

navigation :: ( DomBuilder t m
              , RouteToUrl (R FrontendRoute) m
              , SetRoute t (R FrontendRoute) m
              )
              => FrontendRoute x -> m a -> m a
navigation currentRoute content = do
    E.divC "topnav" $ do
        navItem' FrontendRoute_Main
        navItem' FrontendRoute_About
    E.divC "main" content
        where navItem' route = navItem (samePage currentRoute route) route

navItem :: ( DomBuilder t m
           , RouteToUrl (R FrontendRoute) m
           , SetRoute t (R FrontendRoute) m)
           => Bool -> FrontendRoute () -> m ()
navItem selected linkTo = E.divC cl $ routeLink (linkTo :/ ()) $ text (pageName linkTo)
    where cl = if selected then "nav-item selected"
                           else "nav-item"

header :: (DomBuilder t m) => m ()
header = do
    el "title" $ text "Character Sheet"
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", static @"css/Style.css")]) $ return ()
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", "https://fonts.googleapis.com/css?family=Roboto|Roboto+Mono")]) $ return ()

sheet_body :: ( DomBuilder t m
              , PostBuild t m
              , MonadHold t m
              , MonadFix m
              , MonadJSM m
              , PerformEvent t m
              , MonadJSM (Performable m)
              )
              => m ()
sheet_body = do
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
                , MonadJSM m
                , PerformEvent t m
                , MonadJSM (Performable m)
                )
                => m (Abilities (Dynamic t Int))
abilityBlock = statBlock "Abilities" . grid $ do
    initVals <- fmap readM . liftJSM $ getLocal K.Abilities
    row $ lbl "Ability" >> lbl "Score" >> lbl "Mod"
    abl <- Abilities <$> abilityDisplay (str initVals) "Str"
              <*> abilityDisplay (dex initVals) "Dex"
              <*> abilityDisplay (con initVals) "Con"
              <*> abilityDisplay (wis initVals) "Wis"
              <*> abilityDisplay (int initVals) "Int"
              <*> abilityDisplay (cha initVals) "Cha"
    saveDyn K.Abilities (sequenceA abl)
    return abl
        where
            readM :: Maybe Text -> Abilities Int
            readM mtext = fromMaybe (pure 10) (join $ readMaybe . T.unpack <$> mtext)

abilityDisplay :: ( DomBuilder t m
                  , PostBuild t m
                  ) => Int -> T.Text -> m (Dynamic t Int)
abilityDisplay initialValue name = row $ do
    cell $ text name
    abilityScore <- cell $ fromMaybe 10 <$$> numberInput initialValue
    cellClass "number" $ display (abilityMod <$> abilityScore)
    return abilityScore

classBlock :: (DomBuilder t m
              ,MonadJSM m
              ,PerformEvent t m
              ,MonadJSM (Performable m)
              )
           => m (Dynamic t (ClassData Int))
classBlock = statBlock "Class" . grid $ do
    mCls <- liftJSM $ getLocal K.Class
    let cls = readM mCls
    row $ lbl "Class Name" >> lbl "Level" >> lbl "HP" >> lbl "BAB" >> lbl "Fort" >> lbl "Ref" >> lbl "Will"
    dynCls <- row $ do
        x <- cell . inputElement $ def &
            inputElementConfig_initialValue .~ (className cls)
        let clsName = value x
        levels <- cell $ numDefZero (level cls)
        hp <- cell $ numDefZero (classHealth cls)
        baseAttackBonus <- cell $ numDefZero (bab cls)
        fortSave <- cell $ numDefZero (fortitude cls)
        refSave <- cell $ numDefZero (reflex cls)
        willSave <- cell $ numDefZero (will cls)
        return $ ClassData <$> clsName <*> levels <*> baseAttackBonus <*> fortSave <*> refSave <*> willSave <*> hp
    saveDyn K.Class dynCls
    return dynCls
    where numDefZero initVal = fromMaybe 0 <$$> numberInput initVal
          readM mtext = fromMaybe blankClass (join $ readMaybe . T.unpack <$> mtext)

-- displays current total health, temp hp, wounds, remaining health
healthBlock :: ( DomBuilder t m
               , PostBuild t m
               , PerformEvent t m
               , MonadJSM m
               , MonadJSM (Performable m)
               ) => Abilities (Dynamic t Int) -> Dynamic t (ClassData Int) -> m ()
healthBlock abl cls = statBlock "Health" . grid $ do
    let hp = chHealth <$> (sequenceA abl) <*> cls
    row $ lbl "Max HP" >> lbl "Wounds" >> lbl "HP"
    row $ do
        cellNum (display hp)
        initialWounds <- fmap readM . liftJSM $ getLocal K.Health
        wnds <- cell $ fromMaybe 0 <$$> numberInput initialWounds
        saveDyn K.Health wnds
        cellNum $ display ((-) <$> hp <*> wnds)
    where cellNum = cellClass "number"
          readM mtext = fromMaybe 0 (join $ readMaybe . T.unpack <$> mtext)

combatManuverBlock :: ( DomBuilder t m
                      , PostBuild t m
                      )
                      => Abilities (Dynamic t Int) -> Dynamic t (ClassData Int) -> m ()
combatManuverBlock abl cls = statBlock "Combat Mnvr" . grid $ do
    row $ ct "CMB" >> cellNum (display cmb)
    row $ ct "CMD" >> cellNum (display cmd)
    where combatStats = do
              strengthMod <- str absMod
              dexterity <- dex absMod
              baseAttack <- bab <$> cls
              return $ (strengthMod + baseAttack, strengthMod + dexterity + baseAttack + 10)
          cmb = fst <$> combatStats
          cmd = snd <$> combatStats
          absMod = abilityMod <$$> abl
          cellNum = cellClass "number"

armorBlock :: ( DomBuilder t m
              , MonadHold t m
              , MonadFix m
              , PerformEvent t m
              , MonadJSM m
              , MonadJSM (Performable m)
              )
              => m (Dynamic t Int)
armorBlock = statBlock "Armor" $ mdo
    initialArmor <- fmap readM . liftJSM $ getLocal K.Armor
    let addLines = attachWith (\m _ -> nextKey m =: Just 0) (current armorResults) addPressed
        removeLines = (\k -> k =: Nothing) <$> removeEvents armorResults
    armorResults <- grid $ do
        row $ lbl "name" >> lbl "ac"
        armorRows initialArmor (addLines <> removeLines)
    addPressed <- el "div" $ button "Add"
    let armorVals = joinDynThroughMap (fst <$$> armorResults)
    saveDyn K.Armor armorVals
    return $ sum <$> armorVals
    where removeEvents :: (Reflex t) => Dynamic t (M.Map k (b, Event t a)) -> (Event t a)
          removeEvents x = switchPromptlyDyn $ leftmost . fmap (snd . snd) . M.toList <$> x
          readM mtext = fromMaybe (0 =: 0) (join $ readMaybe . T.unpack <$> mtext)

nextKey :: (Num k) => M.Map k a -> k
nextKey = (1+) . fromMaybe 0 . maxKey

maxKey :: M.Map k a -> Maybe k
maxKey = fmap fst . lookupMax

lookupMax :: M.Map k a -> Maybe (k, a)
lookupMax m | M.null m = Nothing
            | otherwise = Just (M.findMax m)

armorRows :: ( DomBuilder t m
             , MonadHold t m
             , MonadFix m
             )
             => (M.Map Int Int)
             -> Event t (M.Map Int (Maybe Int))
             -> m (Dynamic t (M.Map Int (Dynamic t Int, Event t Int)))
armorRows initValues updates = listWithKeyShallowDiff initValues updates keyedArmorRow

keyedArmorRow :: (DomBuilder t m)
              => Int -> Int -> Event t Int -> m (Dynamic t Int, Event t Int)
keyedArmorRow key initialValue _ = do
          (v, ev) <- armorRow initialValue
          return (v, key <$ ev)

armorRow :: (DomBuilder t m)
            => Int -> m (Dynamic t Int, Event t ())
armorRow initialVal = row $ do
    _ <- cell $ inputElement def
    armorVal <- cell $ fromMaybe 0 <$$> numberInput initialVal
    delEvent <- button "delete line"
    return (armorVal, delEvent)

skillsBlock :: ( DomBuilder t m
               , PostBuild t m
               , PerformEvent t m
               , MonadJSM (Performable m)
               , MonadJSM m
               )
               => Abilities (Dynamic t Int) -> M.Map Text Ability -> m (M.Map Text (Dynamic t Skill))
skillsBlock abl statsConfig = statBlock "Skills" . grid $ do
    initialSkillMap <- fmap readM . liftJSM $ getLocal K.Skill
    row $ lbl "Skill" >> lbl "Ability" >> lbl "Class Skill" >> lbl "Ranks" >> lbl "misc. mod"
        >> lbl "total"
    dSkillMap <- M.traverseWithKey (skillLine abl initialSkillMap) statsConfig
    saveDyn K.Skill (sequenceA dSkillMap)
    return dSkillMap
        where readM mText = fromMaybe (blankSkill <$ statsConfig) (join $ readMaybe . T.unpack <$> mText)

skillLine :: ( DomBuilder t m
             , PostBuild t m
             )
             => Abilities (Dynamic t Int) -> M.Map Text Skill -> Text -> Ability -> m (Dynamic t Skill)
skillLine abls initSkillMap skillTitle abl = row $ do
    let initSkill = fromMaybe blankSkill (M.lookup skillTitle initSkillMap)
    ct skillTitle
    ct . shortName $ abl
    classCB <- cell $ checkbox (isClassSkill initSkill) def
    ranks <- cell $ fromMaybe 0 <$$> numberInput (skillRanks initSkill)
    miscMod <- cell $ fromMaybe 0 <$$> numberInput (skillMod initSkill)
    let sk = Skill <$> pure skillTitle <*> value classCB <*> pure abl <*> ranks <*> miscMod
    cellClass "number" $ display (skillBonus <$> sequenceA abls <*> sk)
    return sk

ct :: (DomBuilder t m) => Text -> m ()
ct = cell . text

lbl :: (DomBuilder t m) => Text -> m ()
lbl = labelCell
