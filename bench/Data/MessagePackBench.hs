module Data.MessagePackBench (suite) where

import           Control.DeepSeq           (NFData)
import           Criterion.Main            (Benchmark, bench, bgroup, nf)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Int                  (Int64)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen       (resize, unGen)
import           Test.QuickCheck.Random    (mkQCGen)

import           Data.MessagePack


defaultSeed :: Int
defaultSeed = 301


arb :: Arbitrary a => Int -> a
arb size =
  let g = unGen $ resize size arbitrary in
  g (mkQCGen defaultSeed) defaultSeed


benchRange
  :: NFData b
  => Int -> Int -> Int -> (a -> b) -> (Int -> a) -> [Benchmark]
benchRange from to steps f g =
  map (\step ->
      let sz = from + ((to - from) `div` (steps - 1)) * step in
      bench (show sz) $ nf f (g sz)
    ) [0..steps-1]


suite :: [Benchmark]
suite =
  [ bgroup "pack"
    [ bench "Just Int" $ nf pack (Just (3 :: Int))
    , bench "Nothing"  $ nf pack (Nothing :: Maybe Int)
    , bench "()"       $ nf pack ()
    , bgroup "[a]" $ benchRange 1000 10000 10 pack (`replicate` ())
      -- ^ should be linear
    ]
  , bgroup "unpack"
    [ bench "Just Int" $ nf (unpack :: LBS.ByteString -> Maybe Int) (pack (Just (3 :: Int)))
    , bench "Nothing"  $ nf (unpack :: LBS.ByteString -> Maybe Int) (pack (Nothing :: Maybe Int))
    , bench "()"       $ nf (unpack :: LBS.ByteString -> Maybe () ) (pack ())
    , bgroup "[a]" $ benchRange 1000 10000 10 pack (`replicate` ())
      -- ^ should be linear
    ]
  ]
