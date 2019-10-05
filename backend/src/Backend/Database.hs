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
import Data.Bits
import Data.Maybe (fromMaybe)
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
    className Text
    spellLevel Int
    deriving Show
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

searchSpells :: (MonadIO m) => SpellSearch -> ReaderT SqlBackend m [Spell]
searchSpells SpellSearch{ prefix } =
    (fmap . fmap) (fromSpellRow . entityVal) $
    select $
    from $ \sp -> do
        let qPrefix = fromMaybe "" prefix <> "%"
        where_ (sp ^. SpellRowName `like` val qPrefix)
        return sp
