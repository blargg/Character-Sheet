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
        , PostBuild t m
        )
     => RoutedT t (R FrontendRoute) m ()
body = subRoute_ $ \x -> do
    navigation x $ case x of
        FrontendRoute_Main -> sheet_body
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
    elAttr "link" (M.fromList [("rel", "shortcut icon"), ("href", static @"image/page_icon.png")]) $ return ()
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", "https://fonts.googleapis.com/css?family=Roboto|Roboto+Mono")]) $ return ()

sheet_body :: ( DomBuilder t m
              , MonadHold t m
              , MonadFix m
              , Prerender js t m
              , PostBuild t m
              )
              => m ()
sheet_body = do
    _ <- characterName
    rec abl <- flex $ do
            abl' <- abilityBlock
            healthBlock abl' cls
            combatManuverBlock abl' cls
            return abl'
        cls <- classBlock
    _ <- armorBlock abl -- armor class
    _ <- skillsBlock abl pathfinderSkills -- skill map
    return ()
    where flex = elClass "div" "flexContainer"

characterName :: (DomBuilder t m, Prerender js t m) => m (Dynamic t Text)
characterName = fmap join . el "h1" $
    prerender
        (text "Loading..." *> pure "")
        (stashValue K.Name $ \minit -> editSpan "characterName" (fromMaybe "Character Name" minit))

abilityBlock :: ( DomBuilder t m
                , MonadHold t m
                , MonadFix m
                , PostBuild t m
                , Prerender js t m
                )
                => m (Dynamic t (Abilities Int))
abilityBlock = prerenderStash K.Abilities abilityBlock'

abilityBlock' :: ( DomBuilder t m
                 , PostBuild t m
                 )
                 => Maybe (Abilities Int) -> m (Dynamic t (Abilities Int))
abilityBlock' minit = statBlock "Abilities" . grid $ do
    let initVals = fromMaybe (pure 10) minit
    row $ lbl "Ability" >> lbl "Score" >> lbl "Mod"
    abl <- Abilities <$> abilityDisplay (str initVals) "Str"
              <*> abilityDisplay (dex initVals) "Dex"
              <*> abilityDisplay (con initVals) "Con"
              <*> abilityDisplay (wis initVals) "Wis"
              <*> abilityDisplay (int initVals) "Int"
              <*> abilityDisplay (cha initVals) "Cha"
    return (sequenceA abl)

abilityDisplay :: ( DomBuilder t m
                  , PostBuild t m
                  ) => Int -> T.Text -> m (Dynamic t Int)
abilityDisplay initialValue name = row $ do
    cell $ text name
    abilityScore <- cell $ fromMaybe 10 <$$> numberInput initialValue
    cellClass "number" $ display (abilityMod <$> abilityScore)
    return abilityScore

classBlock :: ( DomBuilder t m
              , MonadFix m
              , MonadHold t m
              , PostBuild t m
              , Prerender js t m
              )
           => m (Dynamic t (ClassData Int))
classBlock = prerenderStash K.Class classBlock'

classBlock' :: ( DomBuilder t m)
            => Maybe (ClassData Int) -> m (Dynamic t (ClassData Int))
classBlock' minit = statBlock "Class" . grid $ do
    let cls = fromMaybe blankClass minit
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
    return dynCls
    where numDefZero initVal = fromMaybe 0 <$$> numberInput initVal

-- displays current total health, temp hp, wounds, remaining health
healthBlock :: ( DomBuilder t m
               , MonadFix m
               , MonadHold t m
               , PostBuild t m
               , Prerender js t m
               ) => Dynamic t (Abilities Int) -> Dynamic t (ClassData Int) -> m ()
healthBlock abl cls = () <$ prerenderStash K.Health (healthBlock' abl cls)

healthBlock' :: ( DomBuilder t m
                , PostBuild t m
                ) => Dynamic t (Abilities Int) -> Dynamic t (ClassData Int) -> Maybe Int -> m (Dynamic t Int)
healthBlock' abl cls minit = statBlock "Health" . grid $ do
    let hp = chHealth <$> abl <*> cls
    row $ lbl "Max HP" >> lbl "Wounds" >> lbl "HP"
    row $ do
        cellNum (display hp)
        let initialWounds = fromMaybe 0 minit
        wnds <- cell $ fromMaybe 0 <$$> numberInput initialWounds
        cellNum $ display ((-) <$> hp <*> wnds)
        return wnds
    where cellNum = cellClass "number"

combatManuverBlock :: ( DomBuilder t m
                      , PostBuild t m
                      )
                      => Dynamic t (Abilities Int) -> Dynamic t (ClassData Int) -> m ()
combatManuverBlock abl cls = statBlock "Combat Mnvr" . grid $ do
    row $ ct "CMB" >> cellNum (display cmb)
    row $ ct "CMD" >> cellNum (display cmd)
    where combatStats = do
              strengthMod <- str <$> absMod
              dexterity <- dex <$> absMod
              baseAttack <- bab <$> cls
              return $ (strengthMod + baseAttack, strengthMod + dexterity + baseAttack + 10)
          cmb = fst <$> combatStats
          cmd = snd <$> combatStats
          absMod = abilityMod <$$> abl
          cellNum = cellClass "number"

armorBlock :: ( DomBuilder t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m
              , Prerender js t m
              )
              => Dynamic t (Abilities Int) -> m (Dynamic t [Armor Int])
armorBlock abl = prerenderStash K.Armor (armorBlock' abl)

collapseSection :: ( DomBuilder t m
                   , PostBuild t m
                   ) => Dynamic t Closed -> m a -> m a
collapseSection isClosed = elDynAttr "div" dynAttrs
    where attrs Closed = "style" =: "display: none"
          attrs Open = mempty
          dynAttrs = fmap attrs isClosed

armorBlock' :: ( DomBuilder t m
               , PostBuild t m
               , MonadHold t m
               , MonadFix m
               )
               => Dynamic t (Abilities Int)
               -> Maybe [Armor Int] -- initial value (if avail)
               -> m (Dynamic t [Armor Int])
armorBlock' abl minit = statBlock' (text "Armor" *> space "0.5em" *> expandCollapseButton Open) $ \open -> mdo
    E.div $ do
        display dynAc'
        E.spanC "label" (text "AC")
        space "0.5em"
        display dynFlatFoot'
        E.spanC "label" (text "Touch")
    (dynAc', dynFlatFoot', armorList') <- collapseSection open $ mdo
        let initArmorList = fromMaybe [blankArmor] minit
        let initArmorMap = M.fromList $ enumerate initArmorList
        let addLines = attachWith (\m _ -> nextKey m =: Just blankArmor) (current armorResults) addPressed
            removeLines = (\k -> k =: Nothing) <$> removeEvents armorResults
        armorResults <- grid $ do
            row $ lbl "name" >> lbl "ac"
            armorRows initArmorMap (addLines <> removeLines)
        addPressed <- el "div" $ button "Add"
        let armorMap = joinDynThroughMap (fst <$$> armorResults)
        let armorList = snd <$$> M.toList <$> armorMap
        let wornArmor = sum . fmap armorClass <$> armorList
        let dynFlatFoot = (+10) . abilityMod . dex <$> abl
        let dynAc = (+) <$> wornArmor <*> dynFlatFoot
        return (dynAc, dynFlatFoot, armorList)
    return armorList'
    where removeEvents :: (Reflex t) => Dynamic t (M.Map k (b, Event t a)) -> (Event t a)
          removeEvents x = switchPromptlyDyn $ leftmost . fmap (snd . snd) . M.toList <$> x
          enumerate = zip [0..]

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
             => (M.Map Int (Armor Int))
             -> Event t (M.Map Int (Maybe (Armor Int)))
             -> m (Dynamic t (M.Map Int (Dynamic t (Armor Int), Event t Int)))
armorRows initValues updates = listWithKeyShallowDiff initValues updates keyedArmorRow

keyedArmorRow :: (DomBuilder t m)
              => Int -> (Armor Int) -> Event t (Armor Int) -> m (Dynamic t (Armor Int), Event t Int)
keyedArmorRow key initialValue _ = do
          (v, ev) <- armorRow initialValue
          return (v, key <$ ev)

armorRow :: (DomBuilder t m)
            => Armor Int -> m (Dynamic t (Armor Int), Event t ())
armorRow initialVal = row $ do
    armorNm <- fmap value . cell $ inputElement $ def
        & inputElementConfig_initialValue .~ (armorName initialVal)
    armorCls <- cell $ fromMaybe 0 <$$> numberInput (armorClass initialVal)
    delEvent <- buttonC "delete-button" "delete"
    let armorVal = Armor <$> armorNm <*> armorCls
    return (armorVal, delEvent)

skillsBlock :: ( DomBuilder t m
               , MonadFix m
               , MonadHold t m
               , PostBuild t m
               , Prerender js t m
               )
               => Dynamic t (Abilities Int) -> M.Map Text Ability -> m (Dynamic t (M.Map Text Skill))
skillsBlock  abl statsConfg = prerenderStash K.Skill (skillsBlock' abl statsConfg)

skillsBlock' :: ( DomBuilder t m
                , PostBuild t m
                )
               => Dynamic t (Abilities Int)
               -> M.Map Text Ability
               -> Maybe (M.Map Text Skill)
               -> m (Dynamic t (M.Map Text Skill))
skillsBlock' abl statsConfig minit = statBlock "Skills" . grid $ do
    let initialSkillMap = fromMaybe blankSkillsBlock minit
    row $ lbl "Skill" >> lbl "Ability" >> lbl "Class Skill" >> lbl "Ranks" >> lbl "misc. mod"
        >> lbl "total"
    dSkillMap <- sequenceA <$> M.traverseWithKey (skillLine abl initialSkillMap) statsConfig
    return dSkillMap
        where blankSkillsBlock = blankSkill <$ statsConfig

skillLine :: ( DomBuilder t m
             , PostBuild t m
             )
             => Dynamic t (Abilities Int) -> M.Map Text Skill -> Text -> Ability -> m (Dynamic t Skill)
skillLine abls initSkillMap skillTitle abl = row $ do
    let initSkill = fromMaybe blankSkill (M.lookup skillTitle initSkillMap)
    ct skillTitle
    ct . shortName $ abl
    classCB <- cell $ checkbox (isClassSkill initSkill) def
    ranks <- cell $ fromMaybe 0 <$$> numberInput (skillRanks initSkill)
    miscMod <- cell $ fromMaybe 0 <$$> numberInput (skillMod initSkill)
    let sk = Skill <$> pure skillTitle <*> value classCB <*> pure abl <*> ranks <*> miscMod
    cellClass "number" $ display (skillBonus <$> abls <*> sk)
    return sk

ct :: (DomBuilder t m) => Text -> m ()
ct = cell . text

lbl :: (DomBuilder t m) => Text -> m ()
lbl = labelCell
