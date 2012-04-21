module Main where

import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Language.MessagePack.IDL.Test.CodeGen.Cpp.Type

main = defaultMain tests

tests =  [test_cpp_type
         ]

