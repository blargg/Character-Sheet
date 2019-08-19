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

import Control.Monad (void)
import Data.CharacterSheet
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import Reflex.Dom

import qualified Frontend.Elements as E
import qualified Frontend.Materialize as Mat
import Frontend.Layout
import Frontend.Prelude
import Common.Api

import Language.Javascript.JSaddle

spells_page :: forall t m.
               ( DomBuilder t m
               , PostBuild t m
               , MonadJSM (Performable m)
               , HasJSContext (Performable m)
               , PerformEvent t m
               , TriggerEvent t m
               , MonadHold t m
               )
               => m ()
spells_page = do
    search <- searchBox
    pb <- getPostBuild
    let initialLoad = spellRequest (searchText "") <$ pb
    let spellLoadReqEvents = leftmost [fmap spellRequest (updated search), initialLoad ]
    -- event when we load a new set of spells to show
    spellLoad <- fmapMaybe decodeXhrResponse <$> performRequestAsync spellLoadReqEvents :: m (Event t [Spell])
    displayedSpells <- holdDyn [] spellLoad
    spell_list_display displayedSpells
    return ()

searchBox :: (DomBuilder t m) => m (Dynamic t SpellSearch)
searchBox = do
    search_text <- Mat.textInput "spell_search"
    return (searchText <$> search_text)

-- POST a JSON request for a spell list
spellRequest :: SpellSearch -> XhrRequest Text
spellRequest s = postJson "api/spelllist" s

spell_list_display :: ( DomBuilder t m
                      , PostBuild t m) => Dynamic t [Spell] -> m ()
spell_list_display spells = void . dyn $ do
    ss <- spells
    if List.null ss
       then pure $ text "no spells to display"
       else void <$> (mapM_ spell_display <$> spells)

spell_display :: (DomBuilder t m) => Spell -> m ()
spell_display Spell{..} = do
    el "h4" . text $ spellName
    E.div . text $ description
    E.div . spell_levels $ spellLevel
    E.divC "row" $ do
        E.divC "col s6" $ lbl' "Range" >> space' >> E.span (text range)
        E.divC "col s6" $ lbl' "Components" >> space' >> E.span (text . fmtComps $ components)
    E.divC "row" $ do
        E.divC "col s4" $ lbl' "Saving Throw" >> space' >> E.span (text . fmt $ savingThrow)
        E.divC "col s4" $ lbl' "Spell Resist" >> space' >> E.span (text . showT $ spellResist)
        E.divC "col s4" $ lbl' "Duration" >> space' >> E.span (text duration)
    E.divC "row" $ do
        E.divC "col s4" $ lbl' "Target" >> space' >> E.span (text . fmt $ target)
        E.divC "col s4" $ lbl' "Cast Time" >> space' >> E.span (text . fmt $ castTime)

spell_levels :: (DomBuilder t m) => SpellLevelList -> m ()
spell_levels (SpellLevelList sl) = do
    lbl' "spell levels" >> space'
    _ <- mapM f (Map.toList sl)
    return ()
        where f (cl, SpellLevel level) = E.span (text (showT cl)) >> E.span (text (showT level))
