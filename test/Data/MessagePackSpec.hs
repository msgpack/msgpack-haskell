{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Data.MessagePackSpec where

import           Test.Hspec
import           Test.QuickCheck
import qualified Test.QuickCheck.Gen        as Gen

import           Control.Applicative        ((<$>), (<*>))
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Hashable              (Hashable)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Int                   (Int16, Int32, Int64, Int8)
import qualified Data.IntMap                as IntMap
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe
import qualified Data.Text.Lazy             as LT
import           Data.Word                  (Word, Word16, Word32, Word64,
                                             Word8)
import           GHC.Generics               (Generic)

import           Data.MessagePack
import qualified Data.MessagePack.Result    as R


data Unit = Unit
  deriving (Eq, Show, Generic)

instance MessagePack Unit


data Record = Record Int Int Int
  deriving (Eq, Show, Generic)

instance MessagePack Record


data Foo
  = Foo1
  | Foo2 Int
  | Foo3 Int
  | Foo4 Int
  | Foo5 Int
  | Foo6 { unFoo3 :: Int }
  | Foo7 (Maybe Foo)
  | Foo8 Int
  | Foo9 Int Int
  | Foo10 Int Int Int
  deriving (Eq, Show, Generic)

instance MessagePack Foo

instance Arbitrary Foo where
  arbitrary = Gen.oneof
    [ return Foo1
    , Foo2 <$> arbitrary
    , Foo3 <$> arbitrary
    , Foo4 <$> arbitrary
    , Foo5 <$> arbitrary
    , Foo6 <$> arbitrary
    , Foo7 <$> arbitrary
    , Foo8 <$> arbitrary
    , Foo9 <$> arbitrary <*> arbitrary
    , Foo10 <$> arbitrary <*> arbitrary <*> arbitrary
    ]


instance (Hashable k, Ord k, Eq k, Arbitrary k, Arbitrary v)
    => Arbitrary (HashMap.HashMap k v) where
  arbitrary = HashMap.fromList . Map.assocs <$> arbitrary


mid :: MessagePack a => a -> a
mid = Maybe.fromJust . unpack . pack


intMid :: Int64 -> Int64
intMid = mid


coerce :: (MessagePack a, MessagePack b) => a -> Maybe b
coerce = unpack . pack


checkMessage :: Show a => R.Result a -> Expectation
checkMessage (R.Success res) =
  expectationFailure $ "unexpected success: " ++ show res
checkMessage (R.Failure msg) =
  msg `shouldContain` "invalid encoding for "


spec :: Spec
spec = do
  describe "unpack" $
    it "does not throw exceptions on arbitrary data" $
      property $ \bs ->
        case unpack bs of
          Just "" -> return () :: IO ()
          _       -> return () :: IO ()

  describe "failures" $
    it "should contain the same start of the failure message for all types" $ do
      checkMessage (unpack (pack $ ObjectInt (-1)) :: R.Result Foo)
      checkMessage (unpack (pack [ObjectInt (-1), ObjectInt 0]) :: R.Result Foo)
      checkMessage (unpack (pack $ ObjectArray []) :: R.Result Record)
      checkMessage (unpack (pack [0 :: Int, 1, 2, 3]) :: R.Result Record)
      checkMessage (unpack (pack "") :: R.Result Unit)
      checkMessage (unpack (pack "") :: R.Result Record)
      checkMessage (unpack (pack "") :: R.Result ())
      checkMessage (unpack (pack ()) :: R.Result Int)
      checkMessage (unpack (pack ()) :: R.Result Bool)
      checkMessage (unpack (pack ()) :: R.Result Float)
      checkMessage (unpack (pack ()) :: R.Result Double)
      checkMessage (unpack (pack ()) :: R.Result S.ByteString)
      checkMessage (unpack (pack ()) :: R.Result LT.Text)
      checkMessage (unpack (pack "") :: R.Result [String])
      checkMessage (unpack (pack "") :: R.Result (Assoc [(Int, Int)]))
      checkMessage (unpack (pack ()) :: R.Result (Int, Int))
      checkMessage (unpack (pack ()) :: R.Result (Int, Int, Int))
      checkMessage (unpack (pack ()) :: R.Result (Int, Int, Int, Int))
      checkMessage (unpack (pack ()) :: R.Result (Int, Int, Int, Int, Int))
      checkMessage (unpack (pack ()) :: R.Result (Int, Int, Int, Int, Int, Int))
      checkMessage (unpack (pack ()) :: R.Result (Int, Int, Int, Int, Int, Int, Int))
      checkMessage (unpack (pack ()) :: R.Result (Int, Int, Int, Int, Int, Int, Int, Int))
      checkMessage (unpack (pack ()) :: R.Result (Int, Int, Int, Int, Int, Int, Int, Int, Int))

  describe "type coercion" $ do
    it "bool<-int" $
      property $ \(a :: Int) -> coerce a `shouldBe` (Nothing :: Maybe Bool)

    it "int<-bool" $
      property $ \(a :: Bool) -> coerce a `shouldBe` (Nothing :: Maybe Int)

    it "float<-int" $
      property $ \(a :: Int) -> coerce a `shouldBe` Just (fromIntegral a :: Float)
    it "float<-double" $
      property $ \(a :: Double) -> coerce a `shouldBe` Just (realToFrac a :: Float)
    it "float<-string" $
      property $ \(a :: String) -> coerce a `shouldBe` (Nothing :: Maybe Float)

    it "double<-int" $
      property $ \(a :: Int) -> coerce a `shouldBe` Just (fromIntegral a :: Double)
    it "double<-float" $
      property $ \(a :: Float) -> coerce a `shouldBe` Just (realToFrac a :: Double)
    it "double<-string" $
      property $ \(a :: String) -> coerce a `shouldBe` (Nothing :: Maybe Double)

    it "bin<-string" $
      property $ \(a :: S.ByteString) -> coerce a `shouldBe` (Nothing :: Maybe String)

    it "string<-bin" $
      property $ \(a :: String) -> coerce a `shouldBe` (Nothing :: Maybe S.ByteString)

  describe "Identity Properties" $ do
    let sizes = [0xf, 0x10, 0x1f, 0x20, 0xff, 0x100, 0xffff, 0x10000]

    it "unit encoding" $
      Unit `shouldBe` mid Unit

    it "map encodings" $ do
      let rt n = let a = IntMap.fromList [(x, -x) | x <- [0..n]] in a `shouldBe` mid a
      mapM_ rt sizes

    it "list encodings" $ do
      let rt n = let a = replicate n "hello" in a `shouldBe` mid a
      mapM_ rt sizes

    it "string encodings" $ do
      let rt n = let a = replicate n 'a' in a `shouldBe` mid a
      mapM_ rt sizes

    it "bytestring encodings" $ do
      let rt n = let a = S.pack $ replicate n 'a' in a `shouldBe` mid a
      mapM_ rt sizes

    it "ext encodings" $ do
      let rt n = let a = ObjectExt 0 $ S.pack $ replicate n 'a' in a `shouldBe` mid a
      mapM_ rt [0..20]
      mapM_ rt sizes

    it "int encodings" $ do
      (-0x7fffffffffffffff) `shouldBe` intMid (-0x7fffffffffffffff)
      (-0x80000000) `shouldBe` intMid (-0x80000000)
      (-0x7fffffff) `shouldBe` intMid (-0x7fffffff)
      (-0x8000) `shouldBe` intMid (-0x8000)
      (-0x7fff) `shouldBe` intMid (-0x7fff)
      (-1) `shouldBe` intMid (-1)
      0 `shouldBe` intMid 0
      1 `shouldBe` intMid 1
      0x7fff `shouldBe` intMid 0x7fff
      0x8000 `shouldBe` intMid 0x8000
      0x7fffffff `shouldBe` intMid 0x7fffffff
      0x80000000 `shouldBe` intMid 0x80000000
      0x7fffffffffffffff `shouldBe` intMid 0x7fffffffffffffff

    it "int"    $ property $ \(a :: Int   ) -> a `shouldBe` mid a
    it "int8"   $ property $ \(a :: Int8  ) -> a `shouldBe` mid a
    it "int16"  $ property $ \(a :: Int16 ) -> a `shouldBe` mid a
    it "int32"  $ property $ \(a :: Int32 ) -> a `shouldBe` mid a
    it "int64"  $ property $ \(a :: Int64 ) -> a `shouldBe` mid a
    it "word"   $ property $ \(a :: Word  ) -> a `shouldBe` mid a
    it "word8"  $ property $ \(a :: Word8 ) -> a `shouldBe` mid a
    it "word16" $ property $ \(a :: Word16) -> a `shouldBe` mid a
    it "word32" $ property $ \(a :: Word32) -> a `shouldBe` mid a
    it "word64" $ property $ \(a :: Word64) -> a `shouldBe` mid a

    it "ext" $
      property $ \(n, a) -> ObjectExt n a `shouldBe` mid (ObjectExt n a)
    it "nil" $
      property $ \(a :: ()) -> a `shouldBe` mid a
    it "bool" $
      property $ \(a :: Bool) -> a `shouldBe` mid a
    it "float" $
      property $ \(a :: Float) -> a `shouldBe` mid a
    it "double" $
      property $ \(a :: Double) -> a `shouldBe` mid a
    it "string" $
      property $ \(a :: String) -> a `shouldBe` mid a
    it "bytestring" $
      property $ \(a :: S.ByteString) -> a `shouldBe` mid a
    it "lazy-bytestring" $
      property $ \(a :: L.ByteString) -> a `shouldBe` mid a
    it "lazy-text" $
      property $ \(a :: LT.Text) -> a `shouldBe` mid a
    it "maybe int" $
      property $ \(a :: (Maybe Int)) -> a `shouldBe` mid a
    it "[int]" $
      property $ \(a :: [Int]) -> a `shouldBe` mid a
    it "[string]" $
      property $ \(a :: [String]) -> a `shouldBe` mid a
    it "(int, int)" $
      property $ \(a :: (Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int)" $
      property $ \(a :: (Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "(int, int, int, int, int, int, int, int, int)" $
      property $ \(a :: (Int, Int, Int, Int, Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "[(int, double)]" $
      property $ \(a :: [(Int, Double)]) -> a `shouldBe` mid a
    it "[(string, string)]" $
      property $ \(a :: [(String, String)]) -> a `shouldBe` mid a
    it "Assoc [(string, int)]" $
      property $ \(a :: Assoc [(String, Int)]) -> a `shouldBe` mid a
    it "Map String Int" $
      property $ \(a :: Map.Map String Int) -> a `shouldBe` mid a
    it "IntMap Int" $
      property $ \(a :: IntMap.IntMap Int) -> a `shouldBe` mid a
    it "HashMap String Int" $
      property $ \(a :: HashMap.HashMap String Int) -> a `shouldBe` mid a
    it "maybe int" $
      property $ \(a :: Maybe Int) -> a `shouldBe` mid a
    it "maybe nil" $
      property $ \(a :: Maybe ()) -> a `shouldBe` mid a

   -- FIXME: this test is also failing
   --
   -- it should probably be decoded somewhat specially with ObjectExt ?
   --
   -- it "maybe maybe int" $
   --   property $ \(a :: Maybe (Maybe Int)) -> a `shouldBe` mid a
   --
   -- by looking at msgpack specification it looks like Haskells Maybe
   -- type should be probably decoded with custom ObjectExt
   --
    it "maybe bool" $
      property $ \(a :: Maybe Bool) -> a `shouldBe` mid a
    it "maybe double" $
      property $ \(a :: Maybe Double) -> a `shouldBe` mid a
    it "maybe string" $
      property $ \(a :: Maybe String) -> a `shouldBe` mid a
    it "maybe bytestring" $
      property $ \(a :: Maybe S.ByteString) -> a `shouldBe` mid a
    it "maybe lazy-bytestring" $
      property $ \(a :: Maybe L.ByteString) -> a `shouldBe` mid a
    it "maybe [int]" $
      property $ \(a :: Maybe [Int]) -> a `shouldBe` mid a
    it "maybe [string]" $
      property $ \(a :: Maybe [String]) -> a `shouldBe` mid a
    it "maybe (int, int)" $
      property $ \(a :: Maybe (Int, Int)) -> a `shouldBe` mid a
    it "maybe (int, int, int)" $
      property $ \(a :: Maybe (Int, Int, Int)) -> a `shouldBe` mid a
    it "maybe (int, int, int, int)" $
      property $ \(a :: Maybe (Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "maybe (int, int, int, int, int)" $
      property $ \(a :: Maybe (Int, Int, Int, Int, Int)) -> a `shouldBe` mid a
    it "maybe [(int, double)]" $
      property $ \(a :: Maybe [(Int, Double)]) -> a `shouldBe` mid a
    it "maybe [(string, string)]" $
      property $ \(a :: Maybe [(String, String)]) -> a `shouldBe` mid a
    it "maybe (Assoc [(string, int)])" $
      property $ \(a :: Maybe (Assoc [(String, Int)])) -> a `shouldBe` mid a
    it "either int float" $
      property $ \(a :: Either Int Float) -> a `shouldBe` mid a

    it "generics" $
      property $ \(a :: Foo) -> a `shouldBe` mid a
    it "arbitrary message" $
      property $ \(a :: Object) -> a `shouldBe` mid a

  describe "show" $ do
    it "Foo" $ do
      show (toObject Foo1) `shouldBe` "ObjectInt 0"
      show (toObject $ Foo3 3) `shouldBe` "ObjectArray [ObjectInt 2,ObjectInt 3]"
      show (toObject $ Foo9 3 5) `shouldBe` "ObjectArray [ObjectInt 8,ObjectArray [ObjectInt 3,ObjectInt 5]]"
      show (toObject $ Foo10 3 5 7) `shouldBe` "ObjectArray [ObjectInt 9,ObjectArray [ObjectInt 3,ObjectInt 5,ObjectInt 7]]"

    it "Record" $
      show (toObject $ Record 3 5 7) `shouldBe` "ObjectArray [ObjectInt 3,ObjectInt 5,ObjectInt 7]"
