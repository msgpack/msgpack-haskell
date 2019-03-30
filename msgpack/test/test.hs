module Main (main) where

import           Test.Tasty

import           DataCases
import           Properties

main :: IO ()
main = do
  testDataCases <- genDataCases
    [ "10.nil"
    , "11.bool"
    , "12.binary"
    , "20.number-positive"
    , "21.number-negative"
    , "22.number-float"
    , "23.number-bignum"
    , "30.string-ascii"
    , "31.string-utf8"
    , "32.string-emoji"
    , "40.array"
    , "41.map"
    , "42.nested"
    , "50.timestamp"
    , "60.ext"
    ]

  defaultMain (testGroup "Tests" [ idPropTests, testDataCases ])
