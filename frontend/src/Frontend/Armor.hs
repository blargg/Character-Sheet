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
import Control.Monad.Fix
import Data.CharacterSheet
import Frontend.Storage hiding (StorageKey(..))
import qualified Frontend.Storage as K
import Reflex.Dom

import Frontend.Prelude
import qualified Frontend.Bulma as Bulma

armorBlock :: AppWidget t m
              => Dynamic t (Abilities Int) -> m (Dynamic t [Armor Int])
armorBlock abl = stashValue K.Armor (armorBlock' abl)

collapseSection :: ( DomBuilder t m
                   , PostBuild t m
                   ) => Dynamic t Closed -> m a -> m a
collapseSection isClosed = elDynAttr "div" dynAttrs
    where attrs Closed = "style" =: "display: none"
          attrs Open = mempty
          dynAttrs = fmap attrs isClosed

armorBlock' :: forall t m.
               ( DomBuilder t m
               , PostBuild t m
               , MonadHold t m
               , MonadFix m
               )
               => Dynamic t (Abilities Int)
               -> Maybe [Armor Int] -- initial value (if avail)
               -> m (Dynamic t [Armor Int])
armorBlock' abl minit = statBlock' (text "Armor" *> space "0.5em" *> expandCollapseButton Open) $ \open -> mdo
    armorSummary abl wornArmor'
    (wornArmor', armorList') <- collapseSection open $ mdo
        let initArmorList = fromMaybe [blankArmor] minit
        let initArmorMap = M.fromList $ enumerate initArmorList
        let addLines = attachWith (\m _ -> nextKey m =: Just blankArmor) (current armorResults) addPressed
            removeLines = (\k -> k =: Nothing) <$> removeEvents armorResults
        armorResults <- grid $ do
            row $ labelCell "name" >> labelCell "ac" >> labelCell "max dex" >> labelCell "acp" >> labelCell "spell fail%"
            armorRows initArmorMap (addLines <> removeLines)
        addPressed <- el "div" $ Bulma.button "New"
        let armorMap = joinDynThroughMap (fst <$$> armorResults)
        let armorList :: Dynamic t [Armor Int]
            armorList = snd <$$> M.toList <$> armorMap
        let wornArmor :: Dynamic t (ArmorData Int)
            wornArmor = fmap fold $ fmap inner <$> armorList
        return (wornArmor, armorList)
    return armorList'
    where removeEvents :: forall t' k b a. (Reflex t') => Dynamic t' (M.Map k (b, Event t' a)) -> (Event t' a)
          removeEvents x = switchPromptlyDyn $ leftmost . fmap (snd . snd) . M.toList <$> x
          enumerate = zip [0..]

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
        & inputElementConfig_initialValue .~ (name initialVal)
    let initArmor = inner initialVal
    armorCls <- cell $ fromMaybe 0 <$$> numberInput (armorClass initArmor)
    maxDexB <- cell $ numberInput' (maxDexBonus initArmor)
    acp <- cell $ fromMaybe 0 <$$> numberInput (armorCheckPenalty initArmor)
    spellFail <- cell $ fromMaybe (Percentage 0) <$$> percentageInput (arcaneSpellFailChance initArmor)
    delEvent <- buttonC "delete-button" "delete"
    let armorData = ArmorData <$> armorCls <*> maxDexB <*> acp <*> spellFail
    let armorVal = nmd <$> armorNm <*> armorData
    return (armorVal, delEvent)
