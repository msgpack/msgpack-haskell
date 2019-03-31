{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.MessagePack
import           Data.MessagePack.Aeson
import           Test.Tasty
import           Test.Tasty.HUnit

data T
  = A Int String
  | B Double
  deriving (Show, Eq)

deriveJSON defaultOptions ''T

data U
  = C { c1 :: Int, c2 :: String }
  | D { z1 :: Double }
  deriving (Show, Eq)

deriveJSON defaultOptions ''U

data V
  = E String | F
  deriving (Show, Eq)

deriveJSON defaultOptions ''V

data W a
  = G a String
  | H { hHoge :: Int, h_age :: a }
  deriving (Show, Eq)

deriveJSON defaultOptions ''W

test :: (MessagePack a, Show a, Eq a) => a -> IO ()
test v = do
  let bs = pack v
  print bs
  print (unpack bs == Right v)

  let oa = toObject v
  print oa
  print (fromObject oa == Data.MessagePack.Success v)

roundTrip :: (Show a, Eq a, ToJSON a, FromJSON a) => a -> IO ()
roundTrip v = do
  let mp = packAeson v
      v' = unpackAeson mp
  v' @?= pure v

main :: IO ()
main =
  defaultMain $
  testGroup "test case"
  [ testCase "unnamed 1" $
    roundTrip $ A 123 "hoge"
  , testCase "unnamed 2" $
    roundTrip $ B 3.14
  , testCase "named 1" $
    roundTrip $ C 123 "hoge"
  , testCase "named 2" $
    roundTrip $ D 3.14
  , testCase "unit 1" $
    roundTrip $ E "hello"
  , testCase "unit 2" $
    roundTrip F
  , testCase "parameterized 1" $
    roundTrip $ G (E "hello") "world"
  , testCase "parameterized 2" $
    roundTrip $ H 123 F
  ]
