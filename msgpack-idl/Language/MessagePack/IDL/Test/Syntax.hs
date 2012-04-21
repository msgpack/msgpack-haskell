module Language.MessagePack.IDL.Test.Syntax where

import Test.Framework(testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

test_syntax = testGroup "test_syntax"
          [
            testCase "test_trivial_syntax" test_trivial_syntax
          ]

test_trivial_syntax = (assertEqual "" [1] [1])