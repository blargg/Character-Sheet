{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Armor
    ( armorBlock
    ) where

import Common.Compose
import Frontend.Input
import Frontend.Layout
import qualified Frontend.Elements as E

import Data.Foldable (fold)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.CharacterSheet
import Frontend.Storage hiding (StorageKey(..))
import qualified Frontend.Storage as K
import Reflex.Dom

import Frontend.Prelude
import qualified Frontend.Bulma as Bulma

armorBlock :: AppWidget t m
              => Dynamic t (Abilities Int) -> m (Dynamic t [Armor Int])
armorBlock abl = stashValue K.Armor (armorBlock' abl)

armorBlock' :: forall t m.
               (AppWidget t m)
               => Dynamic t (Abilities Int)
               -> Maybe [Armor Int] -- initial value (if avail)
               -> m (Dynamic t [Armor Int])
armorBlock' abl minit = statBlock' (text "Armor" *> space "0.5em" *> expandCollapseButton Open) $ \open -> mdo
    armorSummary abl wornArmor'
    (wornArmor', armorList') <- collapseSection open $ mdo
        let initArmorList = fromMaybe [blankArmor] minit
        let initArmorMap = M.fromList $ enumerate initArmorList
        armorResults <- grid $ do
            row $ labelCell "name" >> labelCell "ac" >> labelCell "max dex" >> labelCell "acp" >> labelCell "spell fail%"
            multiLineWidget initArmorMap blankArmor armorRow
        let armorMap = joinDynThroughMap armorResults
        let armorList :: Dynamic t [Armor Int]
            armorList = snd <$$> M.toList <$> armorMap
        let wornArmor :: Dynamic t (ArmorData Int)
            wornArmor = fmap fold $ fmap inner <$> armorList
        return (wornArmor, armorList)
    return armorList'
    where enumerate = zip [0..]


-- displays a formatted and labeled summary of this armor data
armorSummary :: ( DomBuilder t m
                , PostBuild t m)
                => Dynamic t (Abilities Int) -> Dynamic t (ArmorData Int) -> m ()
armorSummary abl armorData = E.div $ do
    let dynFlatFoot = (+10) . abilityMod . dex <$> abl
    let dynAc = (+) <$> fmap armorClass armorData <*> dynFlatFoot
    display dynAc
    lbl' "AC"
    space'
    display dynFlatFoot
    lbl' "Touch"
    _ <- dyn $ dispMaxDex . maxDexBonus <$> armorData
    space'
    display (armorCheckPenalty <$> armorData)
    lbl' "ACP"
    dispSpellFail (arcaneSpellFailChance <$> armorData)
        where dispMaxDex Nothing = return ()
              dispMaxDex (Just mDex) = do
                  space'
                  text . T.pack . show $ mDex
                  lbl' "Max Dex"
              dispSpellFail sf = do
                  space'
                  dynText (showPercentage <$> sf)
                  lbl' "Spell Fail"

armorRow :: (DomBuilder t m)
            => Armor Int -> m (Dynamic t (Armor Int), Event t ())
armorRow initialVal = row $ do
    armorNm <- fmap value . cell $ inputElement $ def
        & inputElementConfig_initialValue .~ (name initialVal)
    let initArmor = inner initialVal
    armorCls <- cell $ fromMaybe 0 <$$> numberInput (armorClass initArmor)
    maxDexB <- cell $ numberInput' (maxDexBonus initArmor)
    acp <- cell $ fromMaybe 0 <$$> numberInput (armorCheckPenalty initArmor)
    spellFail <- cell $ fromMaybe (Percentage 0) <$$> percentageInput (arcaneSpellFailChance initArmor)
    delEvent <- Bulma.delete
    let armorData = ArmorData <$> armorCls <*> maxDexB <*> acp <*> spellFail
    let armorVal = nmd <$> armorNm <*> armorData
    return (armorVal, delEvent)
