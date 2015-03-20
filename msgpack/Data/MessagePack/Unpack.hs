{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Unpack
-- Copyright : (c) Hideyuki Tanaka, 2009-2015
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- MessagePack Deserializer using @Data.Binary@
--
--------------------------------------------------------------------

module Data.MessagePack.Unpack(
  getNil, getBool, getInt, getFloat, getDouble,
  getRAW, getArray, getMap,
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString                as S
import           Data.Int
import           Data.Word

import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754

getNil :: Get ()
getNil = tag 0xC0

getBool :: Get Bool
getBool =
  False <$ tag 0xC2 <|>
  True  <$ tag 0xC3

getInt :: Get Int
getInt =
  getWord8 >>= \case
    c | c .&. 0x80 == 0x00 ->
        return $ fromIntegral c
      | c .&. 0xE0 == 0xE0 ->
        return $ fromIntegral (fromIntegral c :: Int8)
    0xCC -> fromIntegral <$> getWord8
    0xCD -> fromIntegral <$> getWord16be
    0xCE -> fromIntegral <$> getWord32be
    0xCF -> fromIntegral <$> getWord64be
    0xD0 -> fromIntegral <$> getInt8
    0xD1 -> fromIntegral <$> getInt16be
    0xD2 -> fromIntegral <$> getInt32be
    0xD3 -> fromIntegral <$> getInt64be
    _    -> empty

getFloat :: Get Float
getFloat = tag 0xCA >> getFloat32be

getDouble :: Get Double
getDouble = tag 0xCB >> getFloat64be

getRAW :: Get S.ByteString
getRAW = do
  len <- getWord8 >>= \case
    t | t .&. 0xE0 == 0xA0 ->
      return $ fromIntegral $ t .&. 0x1F
    0xDA -> fromIntegral <$> getWord16be
    0xDB -> fromIntegral <$> getWord32be
    _    -> empty
  getByteString len

getArray :: Get a -> Get [a]
getArray g = do
  len <- getWord8 >>= \case
    t | t .&. 0xF0 == 0x90 ->
      return $ fromIntegral $ t .&. 0x0F
    0xDC -> fromIntegral <$> getWord16be
    0xDD -> fromIntegral <$> getWord32be
    _    -> empty
  replicateM len g

getMap :: Get a -> Get b -> Get [(a, b)]
getMap k v = do
  len <- getWord8 >>= \case
    t | t .&. 0xF0 == 0x80 ->
      return $ fromIntegral $ t .&. 0x0F
    0xDE -> fromIntegral <$> getWord16be
    0xDF -> fromIntegral <$> getWord32be
    _    -> empty
  replicateM len $ (,) <$> k <*> v

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

getInt16be :: Get Int16
getInt16be = fromIntegral <$> getWord16be

getInt32be :: Get Int32
getInt32be = fromIntegral <$> getWord32be

getInt64be :: Get Int64
getInt64be = fromIntegral <$> getWord64be

tag :: Word8 -> Get ()
tag t = do
  b <- getWord8
  guard $ t == b

{-
  -- * MessagePack deserializer
  Unpackable(..),
  -- * Simple function to unpack a Haskell value
  unpack,
  tryUnpack,
  -- * Unpack exception
  UnpackError(..),
  -- * ByteString utils
  IsByteString(..),

-- | Deserializable class
class Unpackable a where
  -- | Deserialize a value
  get :: Get a

-- | The exception of unpack
data UnpackError =
  UnpackError String
  deriving (Show, Typeable)

instance Exception UnpackError

-- | Unpack MessagePack string to Haskell data.
unpack :: Unpackable a => L.ByteString -> a
unpack bs =
  case tryUnpack bs of
    Left err ->
      throw $ UnpackError err
    Right ret ->
      ret

-- | Unpack MessagePack string to Haskell data.
tryUnpack :: (Unpackable a, IsByteString s) => s -> Either String a
tryUnpack bs =
  case A.parse get (toBS bs) of
    A.Fail _ _ err ->
      Left err
    A.Partial _ ->
      Left "not enough input"
    A.Done _ ret ->
      Right ret

instance Unpackable String where
  get = parseString (\n -> return . decodeUtf8 =<< A.take n)

instance Unpackable B.ByteString where
  get = parseString A.take

instance Unpackable BL.ByteString where
  get = parseString (\n -> return . toLBS =<< A.take n)

instance Unpackable T.Text where
  get = parseString (\n -> return . T.decodeUtf8With skipChar =<< A.take n)

instance Unpackable TL.Text where
  get = parseString (\n -> return . TL.decodeUtf8With skipChar . toLBS =<< A.take n)

instance (Unpackable k, Unpackable v) => Unpackable (Assoc [(k,v)]) where
  get = liftM Assoc $ parseMap (flip replicateM parsePair)

instance (Unpackable k, Unpackable v) => Unpackable (Assoc (V.Vector (k, v))) where
  get = liftM Assoc $ parseMap (flip V.replicateM parsePair)

instance (Ord k, Unpackable k, Unpackable v) => Unpackable (M.Map k v) where
  get = parseMap (\n -> M.fromList <$> replicateM n parsePair)

instance Unpackable v => Unpackable (IM.IntMap v) where
  get = parseMap (\n -> IM.fromList <$> replicateM n parsePair)

instance (Hashable k, Eq k, Unpackable k, Unpackable v) => Unpackable (HM.HashMap k v) where
  get = parseMap (\n -> HM.fromList <$> replicateM n parsePair)

instance Unpackable a => Unpackable (Maybe a) where
  get =
    A.choice
    [ liftM Just get
    , liftM (\() -> Nothing) get ]


instance Unpackable a => Unpackable [a] where
  get = parseArray (flip replicateM get)

instance Unpackable a => Unpackable (V.Vector a) where
  get = parseArray (flip V.replicateM get)

instance (Unpackable a1, Unpackable a2) => Unpackable (a1, a2) where
  get = parseArray f where
    f 2 = get >>= \a1 -> get >>= \a2 -> return (a1, a2)
    f n = fail $ printf "wrong tuple size: expected 2 but got %d" n

instance (Unpackable a1, Unpackable a2, Unpackable a3) => Unpackable (a1, a2, a3) where
  get = parseArray f where
    f 3 = get >>= \a1 -> get >>= \a2 -> get >>= \a3 -> return (a1, a2, a3)
    f n = fail $ printf "wrong tuple size: expected 3 but got %d" n

instance (Unpackable a1, Unpackable a2, Unpackable a3, Unpackable a4) => Unpackable (a1, a2, a3, a4) where
  get = parseArray f where
    f 4 = get >>= \a1 -> get >>= \a2 -> get >>= \a3 -> get >>= \a4 -> return (a1, a2, a3, a4)
    f n = fail $ printf "wrong tuple size: expected 4 but got %d" n

instance (Unpackable a1, Unpackable a2, Unpackable a3, Unpackable a4, Unpackable a5) => Unpackable (a1, a2, a3, a4, a5) where
  get = parseArray f where
    f 5 = get >>= \a1 -> get >>= \a2 -> get >>= \a3 -> get >>= \a4 -> get >>= \a5 -> return (a1, a2, a3, a4, a5)
    f n = fail $ printf "wrong tuple size: expected 5 but got %d" n

instance (Unpackable a1, Unpackable a2, Unpackable a3, Unpackable a4, Unpackable a5, Unpackable a6) => Unpackable (a1, a2, a3, a4, a5, a6) where
  get = parseArray f where
    f 6 = get >>= \a1 -> get >>= \a2 -> get >>= \a3 -> get >>= \a4 -> get >>= \a5 -> get >>= \a6 -> return (a1, a2, a3, a4, a5, a6)
    f n = fail $ printf "wrong tuple size: expected 6 but got %d" n

instance (Unpackable a1, Unpackable a2, Unpackable a3, Unpackable a4, Unpackable a5, Unpackable a6, Unpackable a7) => Unpackable (a1, a2, a3, a4, a5, a6, a7) where
  get = parseArray f where
    f 7 = get >>= \a1 -> get >>= \a2 -> get >>= \a3 -> get >>= \a4 -> get >>= \a5 -> get >>= \a6 -> get >>= \a7 -> return (a1, a2, a3, a4, a5, a6, a7)
    f n = fail $ printf "wrong tuple size: expected 7 but got %d" n

instance (Unpackable a1, Unpackable a2, Unpackable a3, Unpackable a4, Unpackable a5, Unpackable a6, Unpackable a7, Unpackable a8) => Unpackable (a1, a2, a3, a4, a5, a6, a7, a8) where
  get = parseArray f where
    f 8 = get >>= \a1 -> get >>= \a2 -> get >>= \a3 -> get >>= \a4 -> get >>= \a5 -> get >>= \a6 -> get >>= \a7 -> get >>= \a8 -> return (a1, a2, a3, a4, a5, a6, a7, a8)
    f n = fail $ printf "wrong tuple size: expected 8 but got %d" n

instance (Unpackable a1, Unpackable a2, Unpackable a3, Unpackable a4, Unpackable a5, Unpackable a6, Unpackable a7, Unpackable a8, Unpackable a9) => Unpackable (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  get = parseArray f where
    f 9 = get >>= \a1 -> get >>= \a2 -> get >>= \a3 -> get >>= \a4 -> get >>= \a5 -> get >>= \a6 -> get >>= \a7 -> get >>= \a8 -> get >>= \a9 -> return (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    f n = fail $ printf "wrong tuple size: expected 9 but got %d" n
-}
