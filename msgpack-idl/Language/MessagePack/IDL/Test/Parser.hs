module Language.MessagePack.IDL.Test.Parser where

import Test.Framework(testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Data.List as L

test_insert1 = (assertEqual "" (L.insert 1 []) [1])
test_insert2 = (assertEqual "" (L.insert 2 [1,3,5]) [1,2,3,5])
test_sort1 = (assertEqual "" (L.sort []::[Int]) [])
test_sort2 = (assertEqual "" (L.sort [1]) [1])
test_sort3 = (assertEqual "" (L.sort [2,1]) [1,2])
test_sort4 = (assertEqual "" (L.sort [3,2,1,4]) [1,2,3,4])

prop_sorted1 xs = (isSorted $ sort xs) == True
  where types = xs::[Int]

isSorted xs = and $ zipWith (<=) xs (drop 1 xs)

test_sort_h =  testGroup "HUnitTests" 
          [
            testCase "test_insert1" test_insert1,
            testCase "test_insert2" test_insert2,
            testCase "test_sort1" test_sort1,
            testCase "test_sort2" test_sort2,
            testCase "test_sort3" test_sort3,
            testCase "test_sort4" test_sort4
          ]

test_sort_q = testGroup "QuickTests" 
          [
            testProperty "test_sorted" prop_sorted1
          ]

