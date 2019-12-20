{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# language DeriveAnyClass #-} must be disabled for persistent to work
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}

module Backend.Database
    ( countPages
    , classColumn
    , countFeatPages
    , createDatabase
    , fromFeatRow
    , fromSpellRow
    , migrateAll
    , searchFeats
    , searchSpells
    , toFeatRow
    , toSpellRow
    ) where

import Control.Monad.Reader
import Data.Foldable (foldl1)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.CharacterSheet hiding (name)

import Backend.Orphans
import Database.Esqueleto
import Database.Persist.TH
import Common.Api
import Common.Prelude

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SpellRow
    name Text
    castTime Text
    components (Set SpellComp)
    description Text
    duration Text
    range Text
    savingThrow SavingThrow
    resist Bool
    target Text
    deriving Show
    bardLvl SpellLevel Maybe
    clericLvl SpellLevel Maybe
    druidLvl SpellLevel Maybe
    paladinLvl SpellLevel Maybe
    rangerLvl SpellLevel Maybe
    sorcererLvl SpellLevel Maybe
    wizardLvl SpellLevel Maybe
FeatRow
    name Text
    description Text
    type FeatType
    prereqs Text
    effect Text
    normalEffect Text
    extra Text
|]

createDatabase :: (MonadIO m) => [Spell] -> ReaderT SqlBackend m ()
createDatabase spells = do
    runMigration migrateAll
    mapM_ (insert . toSpellRow) spells
    return ()

toSpellRow :: Spell -> SpellRow
toSpellRow Spell{..} = SpellRow { spellRowName = spellName
                                , spellRowCastTime = castTime
                                , spellRowComponents = components
                                , spellRowDescription = description
                                , spellRowDuration = duration
                                , spellRowRange = range
                                , spellRowSavingThrow = savingThrow
                                , spellRowResist = spellResist
                                , spellRowTarget = target
                                , spellRowBardLvl = Map.lookup Bard $ toMap spellLevel
                                , spellRowClericLvl = Map.lookup Cleric $ toMap spellLevel
                                , spellRowDruidLvl = Map.lookup Druid $ toMap spellLevel
                                , spellRowPaladinLvl = Map.lookup Paladin $ toMap spellLevel
                                , spellRowRangerLvl = Map.lookup Ranger $ toMap spellLevel
                                , spellRowSorcererLvl = Map.lookup Sorcerer $ toMap spellLevel
                                , spellRowWizardLvl = Map.lookup Wizard $ toMap spellLevel
                                }

fromSpellRow :: SpellRow -> Spell
fromSpellRow SpellRow { spellRowName
                      , spellRowCastTime
                      , spellRowComponents
                      , spellRowDescription
                      , spellRowDuration
                      , spellRowRange
                      , spellRowSavingThrow
                      , spellRowResist
                      , spellRowTarget
                      , spellRowBardLvl
                      , spellRowClericLvl
                      , spellRowDruidLvl
                      , spellRowPaladinLvl
                      , spellRowRangerLvl
                      , spellRowSorcererLvl
                      , spellRowWizardLvl
                      } = Spell { spellName = spellRowName
                                , castTime = spellRowCastTime
                                , description = spellRowDescription
                                , components = spellRowComponents
                                , duration = spellRowDuration
                                , range = spellRowRange
                                , savingThrow = spellRowSavingThrow
                                , spellLevel = SpellLevelList $
                                    toMap Bard spellRowBardLvl
                                 <> toMap Cleric spellRowClericLvl
                                 <> toMap Druid spellRowDruidLvl
                                 <> toMap Paladin spellRowPaladinLvl
                                 <> toMap Ranger spellRowRangerLvl
                                 <> toMap Sorcerer spellRowSorcererLvl
                                 <> toMap Wizard spellRowWizardLvl
                                , spellResist = spellRowResist
                                , target = spellRowTarget
                                }
                                    where toMap cl (Just lvl) = cl =: lvl
                                          toMap _ Nothing = Map.empty

toFeatRow :: Feat -> FeatRow
toFeatRow Feat{..} = FeatRow { featRowName = featName
                             , featRowDescription = featDescription
                             , featRowType = featType
                             , featRowPrereqs = featPrereqs
                             , featRowEffect = featEffect
                             , featRowNormalEffect = normalEffect
                             , featRowExtra = featExtra
                             }

fromFeatRow :: FeatRow -> Feat
fromFeatRow FeatRow{..} = Feat { featName  = featRowName
                               , featDescription = featRowDescription
                               , featType = featRowType
                               , featPrereqs = featRowPrereqs
                               , featEffect = featRowEffect
                               , normalEffect = featRowNormalEffect
                               , featExtra = featRowExtra
                               }

searchSpells :: (MonadIO m) => SpellSearch -> ReaderT SqlBackend m [Spell]
searchSpells spSearch =
    (fmap . fmap) (fromSpellRow . entityVal) $
    select $
    from $ \sp -> do
        filterSpells (query spSearch) sp
        limit spellsPerPage
        offset $ (fromIntegral (page spSearch - 1)) * spellsPerPage
        return sp

spellsPerPage :: (Num a) => a
spellsPerPage = itemsPerPage

itemsPerPage :: (Num a) => a
itemsPerPage = 10

countPages :: (MonadIO m) => SpellSearch -> ReaderT SqlBackend m Int
countPages spSearch = do
    [spCount] <- countSpellRows' spSearch
    return . ceiling $ fromIntegral (unValue spCount) / (spellsPerPage :: Float)

countSpellRows' :: (MonadIO m) => SpellSearch -> ReaderT SqlBackend m [Value Int]
countSpellRows' spSearch =
    select $
    from $ \sp -> do
        filterSpells (query spSearch) sp
        return countRows :: SqlQuery (SqlExpr (Value Int))

filterSpells :: SpellQuery
             -> SqlExpr (Entity SpellRow)
             -> SqlQuery ()
filterSpells SpellQuery{ prefix, searchClass, minLevel, maxLevel } sp = do
    let qPrefix = prefix <> "%"
    where_ (sp ^. SpellRowName `like` val qPrefix)
    case (classColumn =<< searchClass) of
      Just clColumn -> where_ . not_ . isNothing $ sp ^. clColumn
      Nothing -> return ()
    let cls = case searchClass of
                Just cl -> [cl]
                Nothing -> enumAll
    let clCols = mapMaybe classColumn cls
    case (clCols, minLevel) of
      (_:_, Just minLvl) ->
          where_ $
          foldl1 (||.) $
          fmap (\clCol -> (sp ^. clCol) >=. val (Just minLvl)) $
          clCols
      (_, Nothing) -> return ()
      ([], _) -> where_ (val False) -- no spellcasting classes were selected
    case (clCols, maxLevel) of
      (_:_, Just maxLvl) ->
          where_ $
          foldl1 (||.) $
          fmap (\clCol -> (sp ^. clCol) <=. val (Just maxLvl)) $
          clCols
      (_, Nothing) -> return ()
      ([], _) -> where_ (val False) -- no spellcasting classes were selected


searchFeats :: (MonadIO m) => FeatSearch -> ReaderT SqlBackend m [Feat]
searchFeats featSearch =
    (fmap . fmap) (fromFeatRow . entityVal) $
    select $
    from $ \ft -> do
        filterFeats (query featSearch) ft
        limit itemsPerPage
        offset $ (fromIntegral (page featSearch - 1)) * itemsPerPage
        return ft

countFeatPages :: (MonadIO m) => FeatSearch -> ReaderT SqlBackend m Int
countFeatPages ftSearch = do
    [spCount] <- countFeatRows' ftSearch
    return . ceiling $ fromIntegral (unValue spCount) / (spellsPerPage :: Float)

countFeatRows' :: (MonadIO m) => FeatSearch -> ReaderT SqlBackend m [Value Int]
countFeatRows' spSearch =
    select $
    from $ \sp -> do
        filterFeats (query spSearch) sp
        return countRows :: SqlQuery (SqlExpr (Value Int))

filterFeats :: FeatQuery
            -> SqlExpr (Entity FeatRow)
            -> SqlQuery ()
filterFeats FeatQuery{..} ft = do
    let queryPrefix = featNamePrefix <> "%"
    where_ (ft ^. FeatRowName `like` val queryPrefix)

-- gets the column for the specified class
classColumn :: CharacterClass -> Maybe (EntityField SpellRow (Maybe SpellLevel))
classColumn Bard = Just SpellRowBardLvl
classColumn Cleric = Just SpellRowClericLvl
classColumn Druid = Just SpellRowDruidLvl
classColumn Paladin = Just SpellRowPaladinLvl
classColumn Ranger = Just SpellRowRangerLvl
classColumn Sorcerer = Just SpellRowSorcererLvl
classColumn Wizard = Just SpellRowWizardLvl
classColumn _ = Nothing
