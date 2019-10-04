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
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.CharacterSheet hiding (name)

import Database.Esqueleto
import Database.Persist.TH
import Common.Api

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SpellRow
    name String
    castTime Int
    description String
    deriving Show
SpellLevelRow
    spellId SpellRowId
    className String
    spellLevel Int
    deriving Show
|]

createDatabase :: (MonadIO m) => [Spell] -> ReaderT SqlBackend m ()
createDatabase spells = do
    runMigration migrateAll
    mapM_ (insert . toSpellRow) spells
    return ()

toSpellRow :: Spell -> SpellRow
toSpellRow Spell{..} = SpellRow { spellRowName = Text.unpack spellName
                                , spellRowCastTime = fromEnum castTime
                                , spellRowDescription = Text.unpack description
                                }

fromSpellRow :: SpellRow -> Spell
fromSpellRow SpellRow { spellRowName
                      , spellRowCastTime
                      , spellRowDescription
                      } = Spell { spellName = Text.pack spellRowName
                                , castTime = toEnum spellRowCastTime
                                , description = Text.pack spellRowDescription
                                , components = mempty
                                , duration = ""
                                , range = ""
                                , savingThrow = Ref
                                , spellLevel = SpellLevelList mempty
                                , spellResist = True
                                , target = Creature
                                }

searchSpells :: (MonadIO m) => SpellSearch -> ReaderT SqlBackend m [Spell]
searchSpells SpellSearch{ prefix } =
    (fmap . fmap) (fromSpellRow . entityVal) $
    select $
    from $ \sp -> do
        let qPrefix = Text.unpack $ (fromMaybe "" prefix) <> "%"
        where_ (sp ^. SpellRowName `like` val qPrefix)
        return sp


toBitVec :: (Enum e) => Set e -> Int
toBitVec = sum . fmap ((2^) . fromEnum) . Set.toList

fromBitVec :: (Ord e, Enum e) => Int -> Set e
fromBitVec bv = Set.fromList $ fmap (toEnum . fromIntegral) $
    filter (testBit bv) [0..finiteBitSize bv]
