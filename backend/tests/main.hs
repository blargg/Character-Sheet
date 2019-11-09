{-# language TemplateHaskell #-}
module Main where

import System.Exit (exitFailure)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.CharacterSheet
import Backend.Database (classColumn)

prop_every_caster_has_col :: Property
prop_every_caster_has_col = property $ do
    cl <- forAll (Gen.enumBounded :: Gen Class)
    if isSpellCaster cl
       then assert . isJust $ classColumn cl
       else assert . not . isJust $ classColumn cl

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

main :: IO ()
main = do
    pass <- checkParallel $$(discover)
    if pass
       then return ()
       else exitFailure
