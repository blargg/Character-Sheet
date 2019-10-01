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
import qualified Data.Text as Text
import qualified Data.Set as Set
import Reflex.Dom

import qualified Frontend.Elements as E
import qualified Frontend.Materialize as Mat
import Frontend.Layout
import Frontend.Prelude

spells_page :: ( DomBuilder t m
               , PostBuild t m
               )
               => m ()
spells_page = do
    search_text <- Mat.textInput "spell_search"
    let filtered_spells = (do
        t <- search_text
        return $ filter (spellFilter t) exampleSpells)
    spell_list_display filtered_spells
    return ()

spellFilter :: Text -> Spell -> Bool
spellFilter search sp = Text.isPrefixOf search (spellName sp)

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


-- example list of spells for initial testing
exampleSpells :: [Spell]
exampleSpells = [ fireball, ray_of_frost]

fireball :: Spell
fireball = Spell { spellName = "fireball"
                 , spellLevel = SpellLevelList $ Wizard =: SpellLevel 3
                 , description = "shoots fireball at person"
                 , components = Set.fromList [ Verbal, Somantic, Material ]
                 , castTime = StandardAction
                 , duration = "instanteneous"
                 , range = "400ft + 40ft / level"
                 , savingThrow = Ref
                 , spellResist = True
                 , target = Area
                 }

ray_of_frost :: Spell
ray_of_frost = Spell { spellName = "ray of frost"
                 , spellLevel = SpellLevelList $ Wizard =: SpellLevel 0
                 , description = "beam of frost, slows"
                 , components = Set.fromList [ Verbal, Somantic ]
                 , castTime = StandardAction
                 , duration = "instanteneous"
                 , range = "25ft + 5ft / 2 level"
                 , savingThrow = None
                 , spellResist = True
                 , target = Creatures 1
                 }
