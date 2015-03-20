{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Applicative
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.MessagePack
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

instance Arbitrary a => Arbitrary (Assoc a) where
  arbitrary = Assoc <$> arbitrary

instance Arbitrary S.ByteString where
  arbitrary = S.pack <$> arbitrary

instance Arbitrary L.ByteString where
  arbitrary = L.pack <$> arbitrary

mid :: Msgpack a => a -> a
mid = unpack . pack

tests :: TestTree
tests =
  testGroup "Properties"
    [ testProperty "int" $
      \(a :: Int) -> a == mid a
    , testProperty "nil" $
      \(a :: ()) -> a == mid a
    , testProperty "bool" $
      \(a :: Bool) -> a == mid a
    , testProperty "double" $
      \(a :: Double) -> a == mid a
    , testProperty "string" $
      \(a :: String) -> a == mid a
    , testProperty "bytestring" $
      \(a :: S.ByteString) -> a == mid a
    , testProperty "lazy-bytestring" $
      \(a :: L.ByteString) -> a == mid a
    , testProperty "[int]" $
      \(a :: [Int]) -> a == mid a
    , testProperty "[string]" $
      \(a :: [String]) -> a == mid a
--    , testProperty "(int, int)"
--    , testProperty "(int, int, int)"
--    , testProperty "(int, int, int, int)"
--    , testProperty "(int, int, int, int, int)"
--    , testProperty "[(int, double)]"
--    , testProperty "[(string, string)]"
--    , testProperty "Assoc [(string, int)]"
    ]
