{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
module Main(main) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import Data.CharacterSheet (Feat(..), FeatType(..))
import qualified Data.CharacterSheet as CS
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Database.Persist.Sqlite
import GHC.Generics
import System.Environment
import System.Exit
import System.FilePath

import Backend.Database
import Common.Prelude

import Data.Csv

data SpellCsv = SpellCsv { name :: Text
                         , description :: Text
                         , verbal :: Int
                         , somatic :: Int
                         , material :: Int
                         , focus :: Int
                         , divine_focus :: Int
                         , saving_throw :: Text
                         , spell_resistence :: Text
                         , duration :: Text
                         , range :: Text
                         , targets :: Text
                         , casting_time :: Text
                         , bard :: Either Field Int
                         , cleric :: Either Field Int
                         , druid :: Either Field Int
                         , paladin :: Either Field Int
                         , ranger :: Either Field Int
                         , sor :: Either Field Int
                         , wiz :: Either Field Int
                         }
                         deriving (Generic, Show)

instance FromNamedRecord SpellCsv
instance DefaultOrdered SpellCsv

fromCsv :: SpellCsv -> CS.Spell
fromCsv SpellCsv{..} =
    CS.Spell { CS.spellName = name
             , CS.description = description
             , CS.castTime = casting_time
             , CS.savingThrow = parseSavingThrow saving_throw
             , CS.spellLevel = CS.SpellLevelList $
                 spLvl CS.Bard bard
              <> spLvl CS.Cleric cleric
              <> spLvl CS.Druid druid
              <> spLvl CS.Paladin paladin
              <> spLvl CS.Ranger ranger
              <> spLvl CS.Sorcerer sor
              <> spLvl CS.Wizard wiz
             , CS.spellResist = parseSpellResist spell_resistence
             , CS.target = targets
             , CS.range = range
             , CS.duration = duration
             , CS.components = set_exists focus CS.Focus
                            <> set_exists somatic CS.Somatic
                            <> set_exists divine_focus CS.DivineFocus
                            <> set_exists material CS.Material
                            <> set_exists verbal CS.Verbal
             }
                 where set_exists 1 v = Set.singleton v
                       set_exists 0 _ = Set.empty
                       spLvl _ (Left _) = Map.empty
                       spLvl key (Right v) = (key =: CS.SpellLevel v)

parseSavingThrow :: Text -> CS.SavingThrow
parseSavingThrow raw | Text.isInfixOf "will" t = CS.Will
                     | Text.isInfixOf "fort" t = CS.Fort
                     | Text.isInfixOf "ref" t = CS.Ref
                     | otherwise = CS.None
    where t = Text.toLower raw

parseSpellResist :: Text -> Bool
parseSpellResist raw | Text.isPrefixOf "yes" t = True
                     | otherwise = False
    where t = Text.toLower raw


-- | Reads the feat type for the stored CSV
readFeatType :: Text -> Maybe FeatType
readFeatType "Achievement" = Just Achievement
readFeatType "Combat" = Just Combat
readFeatType "General" = Just General
readFeatType "Grit" = Just Grit
readFeatType "Item Creation" = Just ItemCreation
readFeatType "Metamagic" = Just Metamagic
readFeatType "Monster" = Just Monster
readFeatType "Monster, Combat" = Just Monster
readFeatType "Mythic" = Just Mythic
readFeatType "Story" = Just Story
readFeatType _ = Nothing

featTypeParser :: Text -> Parser FeatType
featTypeParser fieldValue = case readFeatType fieldValue of
                              Just ft -> pure ft
                              Nothing -> fail $ "Could not parse \"" ++ Text.unpack fieldValue ++ "\" as FeatType"

newtype FeatCSV = FeatCSV { innerFeat :: Feat }
                       deriving Show

instance FromNamedRecord FeatCSV where
    parseNamedRecord m = fmap FeatCSV $ Feat
        <$> m .: "name"
        <*> m .: "description"
        <*> (m .: "type" >>= featTypeParser)
        <*> m .: "prerequisites"
        <*> m .: "benefit"
        <*> m .: "normal"
        <*> m .: "special"

processFeatFile :: String -> IO ()
processFeatFile filename = do
    csvData <- liftIO $ BL.readFile filename
    case decodeByName csvData of
      Left err -> putStrLn err
      Right (_, v) -> do
          V.forM_ v $ \f ->
              print (f :: FeatCSV)

test :: IO ()
test = processFeatFile "../resources/data/Feats_OGL - Feats_OGL Updated 23-03-2014.csv"

showHelp :: IO ()
showHelp = do
    putStrLn $ "getArgs <DIR>"
    putStrLn $ "    DIR: directory contaning the following files"
    putStrLn $ "    feats.csv: csv file for all the feats to load"
    putStrLn $ "    spells.csv: csv file of all the spells"

generateDB :: String -> IO ()
generateDB directory = do
    let spellsFile = directory </> "spells.csv"
    let featsFile = directory </> "feats.csv"
    putStrLn "Begin spell parser"
    putStrLn spellsFile
    runSqlite "pf.db" $ do
        runMigration migrateAll
        csvData <- liftIO $ BL.readFile spellsFile
        case decodeByName csvData of
          Left err -> liftIO $ putStrLn err
          Right (_, v) -> do
              V.forM_ v $ \p ->
                  insert (toSpellRow $ fromCsv p)
        featData <- liftIO $ BL.readFile featsFile
        case decodeByName featData of
          Left err -> liftIO $ putStrLn err
          Right (_, v) -> do
              V.forM_ v $ \p ->
                  insert (toFeatRow $ innerFeat p)

main :: IO ()
main = do
    args <- getArgs
    case args of
      ("-h":_) -> showHelp
      (name:_) -> generateDB name
      _ -> do
          putStrLn "getArgs <DIR>: DIR must be a valid directory"
