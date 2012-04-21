{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.Test.CodeGen.Cpp.Type where

import Test.Framework(testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Text.Regex.Posix

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Language.MessagePack.IDL.Syntax 
import Language.MessagePack.IDL.CodeGen.Cpp.Type

matchRegex :: LT.Text -> String -> Bool
matchRegex s t = (LT.unpack s)=~t

removeSpace :: LT.Text -> String
removeSpace s = LT.unpack $ LT.replace " " "" s

test_int8  = assertBool "" (matchRegex (genType $ TInt True 8 ) " *int8_t *" )
test_int16 = assertBool "" (matchRegex (genType $ TInt True 16) " *int16_t *")
test_int32 = assertBool "" (matchRegex (genType $ TInt True 32) " *int32_t *")
test_int64 = assertBool "" (matchRegex (genType $ TInt True 64) " *int64_t *")
test_uint8  = assertBool "" (matchRegex (genType $ TInt False 8 ) " *uint8_t *" )
test_uint16 = assertBool "" (matchRegex (genType $ TInt False 16) " *uint16_t *")
test_uint32 = assertBool "" (matchRegex (genType $ TInt False 32) " *uint32_t *")
test_uint64 = assertBool "" (matchRegex (genType $ TInt False 64) " *uint64_t *")
test_string = assertEqual "" (removeSpace $ genType TString) "std::string"
test_simple_vector = assertBool "" (matchRegex (genType $ TList TBool) expected)
  where 
    expected = " *std::vector *< *bool *> *"
test_nested_vector = assertBool "" (matchRegex (genType $ TList $ TList TBool) " *std::vector *< *std::vector *< *bool *>  *> *")
test_simple_map = assertBool "" (matchRegex (genType $ TMap TBool TBool) expected)
  where
    expected = " *std::map *< *bool *, *bool *> *"
test_simple_tuple = assertBool "" (matchRegex (genType $ TTuple [TBool, TBool]) expected)
  where
    expected = " *std::pair *< *bool *, *bool *> *"
test_combination = assertBool "" (matchRegex (genType $ TList $ TTuple [TString, TList TBool]) expected)
  where
    expected = " *std::vector *< *std::pair *< *std::string *, *std::vector *< *bool *>  *>  *> *"

--prop_sorted1 xs = (isSorted $ sort xs) == True
--  where types = xs::[Int]
--isSorted xs = and $ zipWith (<=) xs (drop 1 xs)

test_cpp_type =  testGroup "test of generation of cpp type" 
          [
            testCase "8bit  singed integer"  test_int8,
            testCase "16bit singed integer"  test_int16,
            testCase "32bit singed integer"  test_int32,
            testCase "64bit singed integer"  test_int64,
            testCase "8bit  unsinged integer"  test_uint8,
            testCase "16bit unsinged integer"  test_uint16,
            testCase "32bit unsinged integer"  test_uint32,
            testCase "64bit unsinged integer"  test_uint64,
            testCase "string"        test_string,
            testCase "simple vector" test_simple_vector,
            testCase "nested vector" test_nested_vector,
            testCase "simple map"    test_simple_map,
            testCase "simple tuple"  test_simple_tuple,
            testCase "combination"   test_combination
          ]

--test_sort_q = testGroup "QuickTests" 
--          [
--            testProperty "test_sorted" prop_sorted1
--          ]

