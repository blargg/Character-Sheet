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
    ( createDatabase
    , searchSpells
    ) where

import Control.Monad.Reader
import Data.Map as Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapStrict
import Data.Set (Set)
import Data.Text (Text)
import Data.CharacterSheet hiding (name)

import Database.Esqueleto
import Database.Persist.TH
import Common.Api

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SpellRow
    name Text
    castTime GameDuration
    components (Set SpellComp)
    description Text
    duration Text
    range Text
    savingThrow SavingThrow
    resist Bool
    target Target
    deriving Show
SpellLevelRow
    spellId SpellRowId
    className Class
    spellLevel SpellLevel
    deriving Show
|]

createDatabase :: (MonadIO m) => [Spell] -> ReaderT SqlBackend m ()
createDatabase spells = do
    runMigration migrateAll
    mapM_ insertSpell spells
    return ()

insertSpell :: (MonadIO m) => Spell -> ReaderT SqlBackend m ()
insertSpell sp@Spell{spellLevel = spellLevel} = do
    spellId <- insert $ toSpellRow sp
    mapM_ insert $ (\(c, l) -> SpellLevelRow spellId c l) <$> toList spellLevel

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
                      } = Spell { spellName = spellRowName
                                , castTime = spellRowCastTime
                                , description = spellRowDescription
                                , components = spellRowComponents
                                , duration = spellRowDuration
                                , range = spellRowRange
                                , savingThrow = spellRowSavingThrow
                                , spellLevel = SpellLevelList mempty
                                , spellResist = spellRowResist
                                , target = spellRowTarget
                                }

joinSpell :: SpellRow -> [SpellLevelRow] -> Spell
joinSpell sp spLvls = (fromSpellRow sp){spellLevel = levels}
    where levels = SpellLevelList (Map.fromList levelList)
          levelList = mkTuple <$> spLvls
          mkTuple sl = (spellLevelRowClassName sl, spellLevelRowSpellLevel sl)

joinSpells :: [(Entity SpellRow, Entity SpellLevelRow)] -> [Spell]
joinSpells list = fmap snd . Map.toList $ Map.intersectionWith joinSpell srById lvlById
    where srById = Map.fromList $ fmap (\(sr, _) -> (entityKey sr, entityVal sr)) list
          lvlById = totalFreq $ fmap (\(_, sl) -> (spellLevelRowSpellId . entityVal $ sl, [entityVal sl])) list

-- counts occurences of a with weighted frequency b
totalFreq :: (Ord a, Monoid b) => [(a, b)] -> Map a b
totalFreq list = foldl insertPair Map.empty list
    where insertPair m (key, value) = MapStrict.insertWith (<>) key value m

-- searchSpells :: (MonadIO m) => SpellSearch -> ReaderT SqlBackend m [(Entity SpellRow, Entity SpellLevelRow)]
searchSpells :: (MonadIO m) => SpellSearch -> ReaderT SqlBackend m [Spell]
searchSpells SpellSearch{ prefix, searchClass, minLevel, maxLevel} =
    fmap joinSpells $
    select $
    from $ \(sp `InnerJoin` spLvl) -> do
        on (sp ^. SpellRowId ==. spLvl ^. SpellLevelRowSpellId)
        let qPrefix = prefix <> "%"
        where_ (sp ^. SpellRowName `like` val qPrefix)
        case searchClass of
          Just cl -> where_ (spLvl ^. SpellLevelRowClassName ==. val cl)
          Nothing -> return ()
        case minLevel of
          Just minL -> where_ (spLvl ^. SpellLevelRowSpellLevel >=. val minL)
          Nothing -> return()
        case maxLevel of
          Just maxL -> where_ (spLvl ^. SpellLevelRowSpellLevel <=. val maxL)
          Nothing -> return()
        return (sp, spLvl)
