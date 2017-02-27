{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Data.MessagePackSpec where

import           Test.Hspec
import           Test.QuickCheck
import qualified Test.QuickCheck.Gen           as Gen

import           Control.Applicative           ((<$>), (<*>))
import qualified Data.ByteString.Char8         as S
import qualified Data.ByteString.Lazy          as L8
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                      (Int16, Int32, Int64, Int8)
import qualified Data.IntMap                   as IntMap
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Text.Lazy                as LT
import qualified Data.Vector                   as V
import qualified Data.Vector.Storable          as VS
import qualified Data.Vector.Unboxed           as VU
import           Data.Void                     (Void)
import           Data.Word                     (Word, Word16, Word32, Word64,
                                                Word8)
import           GHC.Generics                  (Generic)

import           Data.MessagePack
import qualified Data.MessagePack.Types.Result as R


data Unit = Unit
  deriving (Eq, Show, Generic)

instance MessagePack Unit


data TyConArgs = TyConArgs Int Int Int
  deriving (Eq, Show, Generic)

instance MessagePack TyConArgs


data Record = Record
  { recordField1 :: Int
  , recordField2 :: Double
  , recordField3 :: String
  }
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

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

instance (Arbitrary a, VS.Storable a) => Arbitrary (VS.Vector a) where
  arbitrary = VS.fromList <$> arbitrary

instance (Arbitrary a, VU.Unbox a) => Arbitrary (VU.Vector a) where
  arbitrary = VU.fromList <$> arbitrary

instance Arbitrary S.ByteString where
  arbitrary = S.pack <$> arbitrary

instance Arbitrary L.ByteString where
  arbitrary = L.pack <$> arbitrary

instance Arbitrary LT.Text where
  arbitrary = LT.pack <$> arbitrary

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

  describe "Assoc" $ do
    it "supports read/show" $
      property $ \(a :: Assoc [(Int, Int)]) ->
        read (show a) `shouldBe` a

    it "inherits ordering from its contained type" $
      property $ \(a :: Assoc Int) b ->
        (unAssoc a < unAssoc b) `shouldBe` (a < b)

  describe "failures" $
    it "should contain the same start of the failure message for all types" $ do
      checkMessage (unpack (pack $ ObjectInt (-1)) :: R.Result Foo)
      checkMessage (unpack (pack [ObjectInt (-1), ObjectInt 0]) :: R.Result Foo)
      checkMessage (unpack (pack $ ObjectArray []) :: R.Result TyConArgs)
      checkMessage (unpack (pack [0 :: Int, 1, 2, 3]) :: R.Result TyConArgs)
      checkMessage (unpack (pack $ ObjectArray []) :: R.Result Record)
      checkMessage (unpack (pack [0 :: Int, 1, 2, 3]) :: R.Result Record)
      checkMessage (unpack (pack "") :: R.Result Unit)
      checkMessage (unpack (pack "") :: R.Result TyConArgs)
      checkMessage (unpack (pack "") :: R.Result Record)
      checkMessage (unpack (pack "") :: R.Result ())
      checkMessage (unpack (pack ()) :: R.Result Int)
      checkMessage (unpack (pack ()) :: R.Result Bool)
      checkMessage (unpack (pack ()) :: R.Result Float)
      checkMessage (unpack (pack ()) :: R.Result Double)
      checkMessage (unpack (pack ()) :: R.Result S.ByteString)
      checkMessage (unpack (pack ()) :: R.Result LT.Text)
      checkMessage (unpack (pack "") :: R.Result [String])
      checkMessage (unpack (pack ()) :: R.Result (V.Vector Int))
      checkMessage (unpack (pack ()) :: R.Result (VS.Vector Int))
      checkMessage (unpack (pack ()) :: R.Result (VU.Vector Int))
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

    it "vector encodings" $ do
      let rt n = let a = V.fromList [0..n] in a `shouldBe` mid a
      mapM_ rt sizes

    it "storable-vector encodings" $ do
      let rt n = let a = VS.fromList [0..n] in a `shouldBe` mid a
      mapM_ rt sizes

    it "unboxed-vector encodings" $ do
      let rt n = let a = VU.fromList [0..n] in a `shouldBe` mid a
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
    it "vector int" $
      property $ \(a :: V.Vector Int) -> a `shouldBe` mid a
    it "storable-vector int" $
      property $ \(a :: VS.Vector Int) -> a `shouldBe` mid a
    it "unboxed-vector int" $
      property $ \(a :: VU.Vector Int) -> a `shouldBe` mid a
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
    it "Option Int" $
      property $ \(a :: Option Int) -> a `shouldBe` mid a
    it "maybe int" $
      property $ \(a :: Maybe Int) -> a `shouldBe` mid a
    it "maybe nil" $
      property $ \(a :: Maybe ()) -> a `shouldBe` mid a
    it "maybe maybe int" $
      property $ \(a :: Maybe (Maybe Int)) -> a `shouldBe` mid a
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

  describe "encoding validation" $ do
    it "word64 2^64-1" $
      pack (0xffffffffffffffff :: Word64) `shouldBe` L8.pack [0xCF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]

    it "decodes empty array as ()" $
      unpack (pack ([] :: [Int])) `shouldBe` Just ()

  describe "show" $ do
    it "Foo" $ do
      show (toObject Foo1) `shouldBe` "ObjectWord 0"
      show (toObject $ Foo3 3) `shouldBe` "ObjectArray [ObjectWord 2,ObjectWord 3]"
      show (toObject $ Foo3 (-3)) `shouldBe` "ObjectArray [ObjectWord 2,ObjectInt (-3)]"
      show (toObject $ Foo9 3 5) `shouldBe` "ObjectArray [ObjectWord 8,ObjectArray [ObjectWord 3,ObjectWord 5]]"
      show (toObject $ Foo9 (-3) (-5)) `shouldBe` "ObjectArray [ObjectWord 8,ObjectArray [ObjectInt (-3),ObjectInt (-5)]]"
      show (toObject $ Foo10 3 5 7) `shouldBe` "ObjectArray [ObjectWord 9,ObjectArray [ObjectWord 3,ObjectWord 5,ObjectWord 7]]"
      show (toObject $ Foo10 (-3) (-5) 7) `shouldBe` "ObjectArray [ObjectWord 9,ObjectArray [ObjectInt (-3),ObjectInt (-5),ObjectWord 7]]"

    it "TyConArgs" $
      show (toObject $ TyConArgs 3 5 7) `shouldBe` "ObjectArray [ObjectWord 3,ObjectWord 5,ObjectWord 7]"

    it "Record" $
      show (toObject $ Record 3 5 "7") `shouldBe` "ObjectArray [ObjectWord 3,ObjectDouble 5.0,ObjectStr \"7\"]"

voidTest :: Void -> Object
voidTest = toObject
