{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Frontend.Armor
import Frontend.Data
import Frontend.Input
import Frontend.Layout
import Frontend.Spells (spells_page)
import Frontend.Storage hiding (StorageKey(..))
import qualified Frontend.Storage as K
import qualified Frontend.About as About
import qualified Frontend.Elements as E
import qualified Frontend.Feats as Feats
import qualified Frontend.Bulma as Bulma
import qualified Frontend.License as License
import qualified Frontend.Inventory as Inventory

import Frontend.Prelude
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
        )
     => RoutedT t (R FrontendRoute) m ()
body = subRoute_ $ \x -> do
    navigation
    case x of
        FrontendRoute_Main -> sheet_body
        FrontendRoute_About -> About.main
        FrontendRoute_License -> License.main

navigation :: ( DomBuilder t m
              , RouteToUrl (R FrontendRoute) m
              , SetRoute t (R FrontendRoute) m
              )
              => m ()
navigation = do
    Bulma.navbar (navItem FrontendRoute_Main) $ do
        navItem FrontendRoute_About
        navItem FrontendRoute_License

navItem :: ( DomBuilder t m
           , RouteToUrl (R FrontendRoute) m
           , SetRoute t (R FrontendRoute) m)
           => FrontendRoute ()
           -> m ()
navItem linkTo =  Bulma.navItem (pageName linkTo) (linkTo :/ ())

header :: (DomBuilder t m) => m ()
header = do
    el "title" $ text "Character Sheet"
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", static @"bulma/bulma.min.css")]) $ return ()
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", static @"css/Style.css")]) $ return ()
    elAttr "link" (M.fromList [("rel", "shortcut icon"), ("href", static @"image/page_icon.png")]) $ return ()
    elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", "https://fonts.googleapis.com/css?family=Roboto|Roboto+Mono")]) $ return ()
    elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") $ return ()

sheet_body :: ( DomBuilder t m
              , Prerender js t m
              )
              => m ()
sheet_body =
    prerender_ loading_page main_tabs

main_tabs :: AppWidget t m => m ()
main_tabs = do
    let tabs = M.fromList [ (1::Int, "Stats")
                          , (2, "Spells")
                          , (3, "Inventory")
                          , (4, "Feats")
                          ]
    tabDyn <- Bulma.tabSelection tabs
    Bulma.section . Bulma.container $ do
        stPage <- displayIf ((==1) <$> tabDyn) $ fadedBack stat_page
        displayIf ((==2) <$> tabDyn) (fadedBack $ spells_page stPage)
        displayIf ((==3) <$> tabDyn) $ fadedBack Inventory.main
        displayIf ((==4) <$> tabDyn) $ fadedBack Feats.main
        return ()
        where fadedBack = E.divC "fadedBack"

-- displays before the page is fully loaded and rendered
loading_page :: (DomBuilder t m) => m ()
loading_page = do
    Bulma.title 1 "Loading"
    Bulma.indeterminateProgress

stat_page :: (AppWidget t m) => m (Dynamic t StatsPageValues)
stat_page = do
    _ <- characterName
    rec abl <- flex $ do
            abl' <- abilityBlock
            healthBlock abl' cls
            combatManuverBlock abl' cls
            initiativeBlock abl'
            return abl'
        cls <- classBlock
    attacksBlock
    _ <- armorBlock abl -- armor class
    _ <- skillsBlock abl pathfinderSkills -- skill map
    return $ StatsPageValues <$> cls
    where flex = elClass "div" "flexContainer"

characterName :: AppWidget t m => m (Dynamic t Text)
characterName = el "h1" $
        (stashValue K.Name $ \minit -> editSpan "characterName" (fromMaybe "Character Name" minit))

abilityBlock :: AppWidget t m
                => m (Dynamic t (Abilities Int))
abilityBlock = stashValue K.Abilities abilityBlock'

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
abilityDisplay initialValue nm = row $ do
    cell $ text nm
    abilityScore <- cell $ fromMaybe 10 <$$> numberInput initialValue
    cellClass "num" $ display (abilityMod <$> abilityScore)
    return abilityScore

classBlock :: AppWidget t m
           => m (Dynamic t (ClassData Int))
classBlock = stashValue K.Class classBlock'

classBlock' :: (AppWidget t m)
            => Maybe (ClassData Int) -> m (Dynamic t (ClassData Int))
classBlock' minit = statBlock "Class" . grid $ do
    let cls = fromMaybe blankClass minit
    row $ lbl "Class" >> lbl "Level" >> lbl "HP" >> lbl "BAB" >> lbl "Fort" >> lbl "Ref" >> lbl "Will"
    dynCls <- row $ do
        cl <- classSelector (cdClass cls)
        levels <- cell $ numDefZero (level cls)
        hp <- cell $ numDefZero (classHealth cls)
        let classLevelStats = classAndLevel <$> cl <*> levels
        cellClass "num" $ display (bab <$> classLevelStats)
        cellClass "num" $ display (fortitude <$> classLevelStats)
        cellClass "num" $ display (reflex <$> classLevelStats)
        cellClass "num" $ display (will <$> classLevelStats)
        return $ ClassData <$> cl <*> levels <*> hp <*> classLevelStats
    return dynCls
    where numDefZero initVal = fromMaybe 0 <$$> numberInput initVal

classSelector :: (AppWidget t m) => CharacterClass -> m (Dynamic t CharacterClass)
classSelector initCl = do
    let classes = M.fromList ((\cl -> (cl, showT cl)) <$> enumAll)
                  :: M.Map CharacterClass Text
    clDropdown <- elClass "div" "select side-margin" $ dropdown initCl (pure classes) def
    return $ _dropdown_value clDropdown

-- displays current total health, temp hp, wounds, remaining health
healthBlock :: AppWidget t m => Dynamic t (Abilities Int) -> Dynamic t (ClassData Int) -> m ()
healthBlock abl cls = () <$ stashValue K.Health (healthBlock' abl cls)

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
    where cellNum = cellClass "num"

combatManuverBlock :: ( DomBuilder t m
                      , PostBuild t m
                      )
                      => Dynamic t (Abilities Int) -> Dynamic t (ClassData Int) -> m ()
combatManuverBlock abl cls = statBlock "Combat Mnvr" . grid $ do
    row $ lbl "CMB" >> cellNum (display cmb)
    row $ lbl "CMD" >> cellNum (display cmd)
    where combatStats = do
              strengthMod <- str <$> absMod
              dexterity <- dex <$> absMod
              baseAttack <- bab . clStats <$> cls
              return $ (strengthMod + baseAttack, strengthMod + dexterity + baseAttack + 10)
          cmb = fst <$> combatStats
          cmd = snd <$> combatStats
          absMod = abilityMod <$$> abl
          cellNum = cellClass "num"

initiativeBlock :: AppWidget t m
                   => Dynamic t (Abilities Int) -> m ()
initiativeBlock abl = () <$ stashValue K.Initiative (initiativeBlock' abl)

initiativeBlock' :: ( DomBuilder t m
                    , MonadFix m
                    , PostBuild t m
                    )
                    => Dynamic t (Abilities Int)
                    -> Maybe Int
                    -> m (Dynamic t Int)
initiativeBlock' abl mbonus = statBlock "Initiative" . grid $ mdo
    let total = initiative' <$> abl <*> bonus
    row $ lbl "total" >> lbl "bonus"
    bonus <- row $ do
        cellClass "num" $ E.span (display total)
        cell $ fromMaybe 0 <$$> numberInput (fromMaybe 0 mbonus)
    return bonus

attacksBlock :: AppWidget t m => m ()
attacksBlock = () <$ stashValue K.Attacks attacksBlock'

attacksBlock' :: AppWidget t m => Maybe [Attack] -> m (Dynamic t [Attack])
attacksBlock' initAttacks = statBlock "Attacks" . grid $ do
    row $ lbl "name" >> lbl "roll" >> lbl "attack bonus"
    let attks = fromMaybe [blankAttack] initAttacks
    join <$> listWidget attks blankAttack attackRowList
        where blankAttack = nmd "" (AttackStats 0 (d 6) 0)
              attackRowList atk = (\x -> [x]) <$$> attackRow atk

attackRow :: (DomBuilder t m) => Attack -> m (Dynamic t Attack)
attackRow attks = do
    let initStats = inner attks
    nameEl <- cell . inputElement $ def &
        inputElementConfig_initialValue .~ (name attks)
    (numDice, diceValue) <- cell $ do
        numDice' <- E.span $ fromMaybe 0 <$$> numberInput (numberOfDice initStats)
        E.span (text "d")
        diceValue' <- E.span $ maybe (d 2) d <$$> numberInput (highestFaceValue . diceKind $ initStats)
        return (numDice', diceValue')
    atkBonus <- cell $ fromMaybe 0 <$$> numberInput (attackBonus initStats)
    let dStats = AttackStats <$> numDice <*> diceValue <*> atkBonus
    return $ nmd <$> value nameEl <*> dStats

skillsBlock :: AppWidget t m
               => Dynamic t (Abilities Int) -> M.Map Text Ability -> m (Dynamic t (M.Map Text Skill))
skillsBlock  abl statsConfg = stashValue K.Skill (skillsBlock' abl statsConfg)

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
    isClassSk <- cell . el "lbl" $ input_checkbox (isClassSkill initSkill) <* E.span (text "")
    ranks <- cell $ fromMaybe 0 <$$> numberInput (skillRanks initSkill)
    miscMod <- cell $ fromMaybe 0 <$$> numberInput (skillMod initSkill)
    let sk = Skill <$> pure skillTitle <*> isClassSk <*> pure abl <*> ranks <*> miscMod
    cellClass "num" $ display (skillBonus <$> abls <*> sk)
    return sk

ct :: (DomBuilder t m) => Text -> m ()
ct = cell . text

lbl :: (DomBuilder t m) => Text -> m ()
lbl = labelCell
