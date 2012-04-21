module Main where

import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Language.MessagePack.IDL.Test.Syntax
import Language.MessagePack.IDL.Test.Parser

main = defaultMain tests

tests =  [
            test_syntax
          , test_sort_h
          , test_sort_q
        ]

