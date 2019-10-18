{-# language TemplateHaskell #-}
module Main where

import System.Exit (exitFailure)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.CharacterSheet

to_from_enum :: (Show a, Eq a, Enum a) => Gen a -> Property
to_from_enum gen = property $ do
    x <- forAll gen
    x === toEnum (fromEnum x)

prop_savingThrow_convert :: Property
prop_savingThrow_convert = to_from_enum $ (Gen.enumBounded :: Gen SavingThrow)

prop_spellComp_convert :: Property
prop_spellComp_convert = to_from_enum $ (Gen.enumBounded :: Gen SpellComp)

prop_GameDuration_convert :: Property
prop_GameDuration_convert = to_from_enum $ (Gen.enumBounded :: Gen GameDuration)

main :: IO ()
main = do
    pass <- checkParallel $$(discover)
    if pass
       then return ()
       else exitFailure
