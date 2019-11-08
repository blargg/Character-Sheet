{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Spells
    ( spells_page
    ) where

import Data.CharacterSheet
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Reflex.Dom

import qualified Frontend.Bulma as Bulma
import qualified Frontend.Elements as E
import Frontend.Input
import Frontend.Layout
import Frontend.Prelude
import qualified Frontend.Storage as Storage
import Common.Api

spells_page :: forall t m.
               AppWidget t m
               => m ()
spells_page = E.divC "columns" $ mdo
    E.divC "column" $ mdo
        preped <- prepared_spells prepEv setPrepedEv
        setPrepedEv <- saved_spell_sets preped
        return ()
    prepEv <- E.divC "column" spell_book
    return ()

spell_book :: forall t m.
              AppWidget t m
              => m (Event t Spell)
spell_book = Bulma.cardClass "page" $ mdo
    Bulma.title 3 "Spell Book"
    query <- searchBox
    pb <- getPostBuild
    let search = PagedSearch <$> query <*> curPage
    let initialLoad = fmap spellRequest $ tag (current search) pb
    let spellLoadReqEvents = leftmost [fmap spellRequest (updated search), initialLoad ]
    -- event when we load a new set of spells to show
    spellLoad <- fmapMaybe decodeXhrResponse <$> performRequestAsync spellLoadReqEvents :: m (Event t SpellSearchResponse)
    spellPage <- holdDyn emptyResponse spellLoad
    let displayedSpells = fmap pageData spellPage
    spellPrepedEv <- spell_list_display displayedSpells
    curPage <- E.divC "margin" $ Bulma.pagination (() <$ updated query) (totalPages <$> spellPage)
    return spellPrepedEv
        where emptyResponse = PagedResponse [] 0 0

prepared_spells :: forall t m.
                   AppWidget t m
                     => Event t Spell
                     -> Event t PrepSet -- ^ clears the current set, and prepares this one
                     -> m (Dynamic t PrepSet)
prepared_spells prepareSpell prepSet = Storage.stashValue Storage.PreparedSpells (prepared_spells' prepareSpell prepSet)

prepared_spells' :: forall t m.
                   AppWidget t m
                     => Event t Spell
                     -> Event t PrepSet
                     -> Maybe PrepSet
                     -> m (Dynamic t PrepSet)
prepared_spells' prepareSpell prepSet mInit = Bulma.cardClass "page" $ mdo
    let initialSet = fromMaybe Map.empty mInit
    let inserts = addCount <$> prepareSpell
    let removes = removeCount <$> castEv
    let clearAndSet = const <$> prepSet
    let updates = leftmost [inserts, removes, clearAndSet] :: Event t (Map Spell Int -> Map Spell Int)
    spell_set <- foldDyn ($) initialSet updates
    castEvs <- dyn $ do
        spell_set_now <- spell_set
        if Map.null spell_set_now
           then return $ no_preped_spells *> pure []
           else return $ preped_spells spell_set_now
    castEv <- switchHold never $ fmap leftmost castEvs :: m (Event t Spell)
    return spell_set

preped_spells :: AppWidget t m => Map Spell Int -> m ([Event t Spell])
preped_spells sps = do
    Bulma.title 3 "Prepared Spells"
    (mapM (uncurry prepedSpell) . Map.toList) sps

no_preped_spells :: (DomBuilder t m) => m ()
no_preped_spells = do
    Bulma.title 3 "Prepare Spells"
    el "p" $ text "Prepare spells from your spell book to see them here"

addCount :: (Ord a) => a -> Map a Int -> Map a Int
addCount key m = Map.insertWith (+) key 1 m

removeCount :: (Ord a) => a -> Map a Int -> Map a Int
removeCount key m = Map.update f key m
    where f x | x <= 1 = Nothing
              | otherwise = Just (x-1)

prepedSpell :: AppWidget t m => Spell -> Int -> m (Event t Spell)
prepedSpell sp remaining = do
    Bulma.hr
    E.div $ spell_display sp
    E.div $ text $ showT remaining <> " Prepared"
    castEv <- E.div $ Bulma.button "Cast"
    return $ sp <$ castEv

saved_spell_sets :: AppWidget t m => Dynamic t PrepSet -> m (Event t PrepSet)
saved_spell_sets prepared = Bulma.cardClass "page top-margin" $ do
    Bulma.title 3 "Saved Spell Sets"
    prepEv <- saved_set (current prepared) "Spell Set 1"
    return prepEv

saved_set :: AppWidget t m => Behavior t PrepSet -> Text -> m (Event t PrepSet)
saved_set currentPreped setName = do
    E.div $ text setName
    (prepEv, saveEv) <- E.divC "field is-grouped" $ do
        prepEv' <- E.divC "control" $ Bulma.buttonPrimary "Prep"
        saveEv' <- E.divC "control" $ Bulma.button "Save"
        return (prepEv', saveEv')
    initialSaved <- fromMaybe Map.empty <$> Storage.loadJson Storage.SpellSets
    savedSet <- holdDyn initialSaved (tag currentPreped saveEv)
    Storage.saveDyn Storage.SpellSets savedSet
    return $ tag (current savedSet) prepEv

searchBox :: ( DomBuilder t m
             , MonadFix m
             , MonadHold t m
             , PostBuild t m
             ) => m (Dynamic t SpellQuery)
searchBox = E.divC "control" $ do
    search_text <- E.divC "field" $ Bulma.textInput "Search"
    let classes = Map.fromList ((\cl -> (Just cl, showT cl)) <$> filter isSpellCaster enumAll)
                  <> (Nothing =: "Any Class")
                  :: Map (Maybe Class) Text
    cl <- E.divC "field" $ elClass "div" "select" $ dropdown Nothing (pure classes) def
    (minLevel, maxLevel) <- E.divC "field" $ do
        lbl' "min level"
        minL <- numberInput' Nothing
        space'
        lbl' "max level"
        maxL <- numberInput' Nothing
        return (minL, maxL)
    return $ SpellQuery
        <$> search_text
        <*> _dropdown_value cl
        <*> (fmap . fmap) SpellLevel minLevel
        <*> (fmap . fmap) SpellLevel maxLevel

-- POST a JSON request for a spell list
spellRequest :: SpellSearch -> XhrRequest Text
spellRequest s = postJson "api/spelllist" s

spell_list_display :: forall t m.
                      AppWidget t m => Dynamic t [Spell] -> m (Event t Spell)
spell_list_display spells = do
    updatingPrepEv <- dyn $ do
        ss <- spells
        return $ if List.null ss
           then empty_spell_list *> return never
           else leftmost <$> (mapM spellbook_spell ss)
    switchHold never updatingPrepEv

empty_spell_list :: (DomBuilder t m) => m ()
empty_spell_list = do
    Bulma.title 4 "No spells found"

spellbook_spell :: AppWidget t m => Spell -> m (Event t Spell)
spellbook_spell sp = do
    Bulma.hr
    spell_display sp
    prep <- E.div $ Bulma.button "Prepare"
    return $ sp <$ prep

spell_display :: AppWidget t m => Spell -> m ()
spell_display Spell{..} = mdo
    Bulma.titleClass "is-marginless" 5 spellName
    collapseSection expanded $ do
        E.div . text $ description
        E.divC "flexContainer" $ do
            E.divC "side-margin" $ do
                E.div $ lbl' "Cast Time" >> space' >> E.span (text . fmt $ castTime)
                E.div $ lbl' "Components" >> space' >> E.span (text . fmtComps $ components)
                E.div $ lbl' "Duration" >> space' >> E.span (text duration)
                E.div $ lbl' "Range" >> space' >> E.span (text range)
            E.divC "side-margin" $ do
                E.div $ lbl' "Saving Throw" >> space' >> E.span (text . fmt $ savingThrow)
                E.div . spell_levels $ spellLevel
                E.div $ lbl' "Spell Resist" >> space' >> E.span (text . showT $ spellResist)
                E.div $ lbl' "Target" >> space' >> E.span (text . fmt $ target)
    expanded <- expandCollapseText Closed
    return ()

spell_levels :: (DomBuilder t m) => SpellLevelList -> m ()
spell_levels (SpellLevelList sl) = do
    lbl' "Spell Levels" >> space'
    _ <- mapM f (Map.toList sl)
    return ()
        where f (cl, SpellLevel level) = E.span (text (showT cl)) >> E.span (text (showT level))
