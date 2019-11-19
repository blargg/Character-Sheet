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

import Control.Monad (join)
import Data.CharacterSheet
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Reflex.Dom

import qualified Frontend.Bulma as Bulma
import qualified Frontend.Elements as E
import Frontend.Data
import Frontend.Input
import Frontend.Layout
import Frontend.Prelude
import qualified Frontend.Storage as Storage
import Common.Api

spells_page :: forall t m.
               AppWidget t m
               => Dynamic t StatsPageValues
               -> m ()
spells_page statPg = E.divC "columns" $ mdo
    E.divC "column" $ mdo
        preped <- prepared_spells prepEv setPrepedEv
        setPrepedEv <- saved_spell_sets preped
        return ()
    prepEv <- E.divC "column" (spell_book statPg)
    return ()

spell_book :: forall t m.
              AppWidget t m
              => Dynamic t StatsPageValues
              -> m (Event t Spell)
spell_book statPg = Bulma.cardClass "page" $ mdo
    Bulma.title 3 "Spell Book"
    query <- searchBox statPg
    Bulma.hr
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
    let rightSide = do
            Bulma.levelItem $ text $ showT remaining <> " Prepared"
            Bulma.levelItem $ Bulma.button "Cast"
    castEv <- E.div $ spell_display sp rightSide
    return $ sp <$ castEv

saved_spell_sets :: AppWidget t m => Dynamic t PrepSet -> m (Event t PrepSet)
saved_spell_sets prepared = Bulma.cardClass "page top-margin" $ do
    Bulma.title 3 "Saved Spell Sets"
    initialSaved <- fromMaybe emptyList <$> Storage.loadJson Storage.SpellSets
    dynRs <- listWidget' initialSaved newSpellSet (saved_set (current prepared))
    let prepSets = join $ fmap (fst) dynRs
    Storage.saveDyn Storage.SpellSets prepSets
    let prepEvs = switchDyn $ fmap (snd) dynRs
    return (fmap head prepEvs)
        where emptyList = [ newSpellSet ]
              newSpellSet = nmd "New Spell Set" Map.empty

saved_set :: AppWidget t m
          => Behavior t PrepSet
          -> Nmd PrepSet
          -> m ((Dynamic t [Nmd PrepSet], Event t [PrepSet]), Event t ())
saved_set currentPreped initialSaved = Bulma.level $ do
    dynName <- Bulma.levelLeft . Bulma.levelItem $ textSimple (name initialSaved)
    (prepEv, saveEv, delEv) <- Bulma.levelRight $ do
        prepEv' <- Bulma.levelItem $ Bulma.buttonPrimary "Prep"
        saveEv' <- Bulma.levelItem $ Bulma.button "Save"
        delEv' <- Bulma.levelItem $ Bulma.button "Delete"
        return (prepEv', saveEv', delEv')
    savedSet <- holdDyn (inner initialSaved) (tag currentPreped saveEv)
    let setWithName = nmd <$> dynName <*> savedSet
    let prepEvents = tag (current savedSet) prepEv
    return ((fSingle setWithName, fSingle prepEvents), delEv)

fSingle :: (Functor f) => f a -> f [a]
fSingle = fmap (\x -> [x])

searchBox :: ( AppWidget t m) => Dynamic t StatsPageValues
               -> m (Dynamic t SpellQuery)
searchBox statPg = E.divC "control" $ do
    search_text <- E.divC "field" $ Bulma.textInput "Search"
    let clrTabs = Map.fromList $ [ (1::Int, "Manual")
                                 , (2, "Auto")
                                 ]
    clrTab <- Bulma.tabSelection clrTabs
    crm <- displayIf ((==1) <$> clrTab) manualClassRestriction
    cra <- displayIf ((==2) <$> clrTab) $ autoClassRestriction statPg
    let classRestriction = do
        cTab <- clrTab
        case cTab of
          1 -> crm
          2 -> cra
          _ -> undefined
    return $ mkSearch
        <$> search_text
        <*> classRestriction

mkSearch :: Text -> ClassRestriction -> SpellQuery
mkSearch search_text ClassRestriction{..} = SpellQuery { prefix = search_text
                                                       , searchClass = rstrClass
                                                       , minLevel = rstrMinLvl
                                                       , maxLevel = rstrMaxLvl
                                                       }

data ClassRestriction = ClassRestriction { rstrClass :: Maybe CharacterClass
                                         , rstrMinLvl :: Maybe SpellLevel
                                         , rstrMaxLvl :: Maybe SpellLevel
                                         }

manualClassRestriction :: (AppWidget t m) => m (Dynamic t ClassRestriction)
manualClassRestriction =  do
    let classes = Map.fromList ((\cl -> (Just cl, showT cl)) <$> filter isSpellCaster enumAll)
                  <> (Nothing =: "Any Class")
                  :: Map (Maybe CharacterClass) Text
    cl <- E.divC "field" $ elClass "div" "select" $ dropdown Nothing (pure classes) def
    (minLevel, maxLevel) <- E.divC "field" $ do
        lbl' "min level"
        minL <- numberInput' noValue
        space'
        lbl' "max level"
        maxL <- numberInput' noValue
        return (minL, maxL)
    return $ ClassRestriction
        <$> _dropdown_value cl
        <*> (fmap . fmap) SpellLevel minLevel
        <*> (fmap . fmap) SpellLevel maxLevel

autoClassRestriction :: (AppWidget t m)
                     => Dynamic t StatsPageValues
                     -> m (Dynamic t ClassRestriction)
autoClassRestriction statsPg = do
    dynText $ fmap (describeClass) statsPg
    return $ mkRestriction <$> statsPg
        where mkRestriction (StatsPageValues ClassData{..}) = ClassRestriction { rstrClass = Just cdClass
                                                                               , rstrMinLvl = Nothing
                                                                               , rstrMaxLvl = Just $ maxSpellLevel cdClass level
                                                                               }
              describeClass (StatsPageValues ClassData{..}) = let (SpellLevel lvl) = maxSpellLevel cdClass level
                                                               in if isSpellCaster cdClass
                                                                  then showT cdClass <> " spells, up to spell level " <> showT lvl
                                                                  else "Set the class for your character to a spellcaster on the Stats tab."

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
    let prepButton = Bulma.button "Prepare"
    prep <- spell_display sp prepButton
    return $ sp <$ prep

spell_display :: AppWidget t m => Spell -> m a -> m a
spell_display Spell{..} rightLevel = mdo
    (expanded, result) <- Bulma.level $ do
        left <- Bulma.levelLeft $ do
            Bulma.levelItem $ Bulma.titleClass "is-marginless" 5 spellName
            Bulma.levelItem $ expandCollapseText Closed
        right <- Bulma.levelRight $ rightLevel
        return (left, right)
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
    return result

spell_levels :: (DomBuilder t m) => SpellLevelList -> m ()
spell_levels (SpellLevelList sl) = do
    lbl' "Spell Levels" >> space'
    _ <- mapM f (Map.toList sl)
    return ()
        where f (cl, SpellLevel level) = E.span (text (showT cl)) >> E.span (text (showT level))
