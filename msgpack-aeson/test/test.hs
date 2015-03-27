{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables, TemplateHaskell #-}

import Data.MessagePack
import Data.MessagePack.Aeson
import Data.Aeson.TH

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
  print (unpack bs == Just v)

  let oa = toObject v
  print oa
  print (fromObject oa == Just v)

main :: IO ()
main = do
  test $ AsMessagePack $ A 123 "hoge"
  test $ AsMessagePack $ B 3.14
  test $ AsMessagePack $ C 123 "hoge"
  test $ AsMessagePack $ D 3.14
  test $ AsMessagePack $ E "hello"
  test $ AsMessagePack   F
  test $ AsMessagePack $ G (E "hello") "world"
  test $ AsMessagePack $ H 123 F
