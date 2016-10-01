module Main (main) where

import           Criterion.Main            (bgroup, defaultMain)
import           Test.QuickCheck.Random    (mkQCGen)

import qualified Data.MessagePack.IntBench
import qualified Data.MessagePackBench


main :: IO ()
main = defaultMain
  [ bgroup "Data.MessagePack" Data.MessagePackBench.suite
  , bgroup "Data.MessagePack.Int" Data.MessagePack.IntBench.suite
  ]
