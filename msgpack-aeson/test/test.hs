{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Monad
import           Data.Aeson             as A
import           Data.Aeson.TH
import           Data.Int
import           Data.MessagePack       as MP
import           Data.MessagePack.Aeson
import           Data.Word
import           GHC.Generics           (Generic)
import           Test.Tasty
import           Test.Tasty.HUnit

data T
  = A Int String
  | B Double
  deriving (Show, Eq, Generic)

instance FromJSON T; instance ToJSON T

data U
  = C { c1 :: Int, c2 :: String }
  | D { z1 :: Double }
  deriving (Show, Eq, Generic)

instance FromJSON U; instance ToJSON U

data V
  = E String | F
  deriving (Show, Eq, Generic)

instance FromJSON V; instance ToJSON V

data W a
  = G a String
  | H { hHoge :: Int, h_age :: a }
  deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (W a); instance ToJSON a => ToJSON (W a)

instance (FromJSON a, ToJSON a) => MessagePack (W a) where
  toObject = unsafeViaToJSON
  fromObject = viaFromJSON

test :: (MessagePack a, Show a, Eq a) => a -> IO ()
test v = do
  let bs = pack v
  print bs
  print (unpack bs == Right v)

  let oa = toObject v
  print oa
  print (fromObject oa == MP.Success v)

roundTrip :: (Show a, Eq a, ToJSON a, FromJSON a) => a -> IO ()
roundTrip v = do
  let mp = packAeson v
      v' = case mp of
        MP.Error e   -> A.Error e
        MP.Success a -> unpackAeson a
  v' @?= pure v

roundTrip' :: (Show a, Eq a, MessagePack a) => a -> IO ()
roundTrip' v = (unpack . pack $ v) @?= pure v

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
    roundTrip' $ G (E "hello") "world"
  , testCase "parameterized 2" $
    roundTrip' $ H 123 F
  , testCase "negative numbers" $
    roundTrip $ Number $ fromIntegral (minBound :: Int64)
  , testCase "positive numbers" $
    roundTrip $ Number $ fromIntegral (maxBound :: Word64)
  , testCase "big negative" $
    (fromAeson . Number $ -9223372036854775936) @?= (MP.Error "number out of bounds")
  , testCase "big positive" $
    (fromAeson . Number $ 999223372036854775936) @?= (MP.Error "number out of bounds")
  , testCase "double precision" $
    roundTrip . Number $ 10.0
  , testCase "really big integer" $
    (fromAeson . Number $ read "1.0e999999") @?= (MP.Error "number out of bounds")
    -- high precision decimals silently lose precision
  ]
