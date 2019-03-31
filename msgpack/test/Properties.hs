{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Properties (idPropTests) where

import           Control.Applicative        as App
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Int
import           Data.Maybe
import           Data.Word
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.MessagePack
import           Data.MessagePack.Timestamp

instance Arbitrary a => Arbitrary (Assoc a) where
  arbitrary = Assoc App.<$> arbitrary

instance Arbitrary S.ByteString where
  arbitrary = S.pack <$> arbitrary

instance Arbitrary L.ByteString where
  arbitrary = L.pack <$> arbitrary

instance Arbitrary MPTimestamp where
  arbitrary = frequency
    [ (1, fromJust . mptsFromPosixNanoseconds <$> choose (mptsToPosixNanoseconds minBound, mptsToPosixNanoseconds maxBound))
    , (1, mptsFromPosixSeconds <$> arbitrary)
    , (1, fromJust . mptsFromPosixNanoseconds <$> choose (0, 0x400000000 * 1000000000))
    ]

mid :: MessagePack a => a -> a
mid = either error id . unpack . pack

idPropTests :: TestTree
idPropTests = testGroup "Identity Properties"
    [ testProperty "int" $
      \(a :: Int) -> a == mid a
    , testProperty "word" $
      \(a :: Word) -> a == mid a
    , testProperty "nil" $
      \(a :: ()) -> a == mid a
    , testProperty "bool" $
      \(a :: Bool) -> a == mid a
    , testProperty "float" $
      \(a :: Float) -> a == mid a
    , testProperty "double" $
      \(a :: Double) -> a == mid a
    , testProperty "string" $
      \(a :: String) -> a == mid a
    , testProperty "bytestring" $
      \(a :: S.ByteString) -> a == mid a
    , testProperty "lazy-bytestring" $
      \(a :: L.ByteString) -> a == mid a
    , testProperty "maybe int" $
      \(a :: (Maybe Int)) -> a == mid a
    , testProperty "[int]" $
      \(a :: [Int]) -> a == mid a
    , testProperty "[()]" $
      \(a :: [()]) -> a == mid a
    , testProperty "[string]" $
      \(a :: [String]) -> a == mid a
    , testProperty "(int, int)" $
      \(a :: (Int, Int)) -> a == mid a
    , testProperty "(int, int, int)" $
      \(a :: (Int, Int, Int)) -> a == mid a
    , testProperty "(int, int, int, int)" $
      \(a :: (Int, Int, Int, Int)) -> a == mid a
    , testProperty "(int8, int16, int32, int64)" $
      \(a :: (Int8, Int16, Int32, Int64)) -> a == mid a
    , testProperty "(word,word8, word16, word32, word64)" $
      \(a :: (Word, Word8, Word16, Word32, Word64)) -> a == mid a
    , testProperty "(int, int, int, int, int)" $
      \(a :: (Int, Int, Int, Int, Int)) -> a == mid a
    , testProperty "[(int, double)]" $
      \(a :: [(Int, Double)]) -> a == mid a
    , testProperty "[(string, string)]" $
      \(a :: [(String, String)]) -> a == mid a
    , testProperty "Assoc [(string, int)]" $
      \(a :: Assoc [(String, Int)]) -> a == mid a
    , testProperty "MPTimestamp" $
      \(a :: MPTimestamp) -> a == mid a
    , testProperty "maybe (Int,Bool,String)" $
      \(a :: (Maybe ((),Maybe Int,Maybe Float,Maybe Bool,Maybe Double,Maybe String))) -> a == mid a
    ]
