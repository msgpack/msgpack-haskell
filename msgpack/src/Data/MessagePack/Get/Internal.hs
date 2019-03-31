{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Get
-- Copyright : © Hideyuki Tanaka 2009-2015
--           , © Herbert Valerio Riedel 2019
-- License   : BSD3
--
-- MessagePack Deserializer using "Data.Binary"
--
--------------------------------------------------------------------

module Data.MessagePack.Get.Internal
  ( getNil
  , getBool

  , getFloat
  , getDouble

  , getInt
  , getWord
  , getInt64
  , getWord64

  , getStr
  , getBin

  , getArray
  , getMap

  , getExt
  , getExt'
  ) where

import           Compat.Prelude

import qualified Data.ByteString          as S
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Vector              as V

import           Compat.Binary
import           Data.MessagePack.Integer
import           Data.MessagePack.Tags

getNil :: Get ()
getNil = tag TAG_nil

getBool :: Get Bool
getBool =
  getWord8 >>= \case
    TAG_false -> return False
    TAG_true  -> return True
    _         -> empty

getFloat :: Get Float
getFloat = tag TAG_float32 >> getFloat32be

getDouble :: Get Double
getDouble = tag TAG_float64 >> getFloat64be

-- local helper for single-tag decoders
tag :: Word8 -> Get ()
tag t = do { b <- getWord8; guard (t == b) }

-- | Deserialize an integer into an 'Int'
--
-- This operation will fail if the encoded integer doesn't fit into the value range of the 'Int' type.
--
-- @since 1.1.0.0
getInt :: Get Int
getInt = maybe empty pure =<< fromMPInteger <$> get

-- | Deserialize an integer into a 'Word'
--
-- This operation will fail if the encoded integer doesn't fit into the value range of the 'Word' type.
--
-- @since 1.0.1.0
getWord :: Get Word
getWord = maybe empty pure =<< fromMPInteger <$> get

-- | Deserialize an integer into an 'Int64'
--
-- This operation will fail if the encoded integer doesn't fit into the value range of the 'Int64' type.
--
-- @since 1.0.1.0
getInt64 :: Get Int64
getInt64 = maybe empty pure =<< fromMPInteger <$> get

-- | Deserialize an integer into a 'Word'
--
-- This operation will fail if the encoded integer doesn't fit into the value range of the 'Word64' type.
--
-- @since 1.0.1.0
getWord64 :: Get Word64
getWord64 = maybe empty pure =<< fromMPInteger <$> get

getStr :: Get T.Text
getStr = do
  len <- getWord8 >>= \case
    t | Just sz <- is_TAG_fixstr t -> pure sz
    TAG_str8  -> intCast <$> getWord8
    TAG_str16 -> intCast <$> getWord16be
    TAG_str32 -> getWord32be
    _         -> empty

  len' <- fromSizeM "getStr: data exceeds capacity of ByteString/Text" len
  bs <- getByteString len'
  case T.decodeUtf8' bs of
    Left _  -> empty
    Right v -> return v

getBin :: Get S.ByteString
getBin = do
  len <- getWord8 >>= \case
    TAG_bin8  -> intCast <$> getWord8
    TAG_bin16 -> intCast <$> getWord16be
    TAG_bin32 -> getWord32be
    _         -> empty
  len' <- fromSizeM "getBin: data exceeds capacity of ByteString" len
  getByteString len'

getArray :: Get a -> Get (V.Vector a)
getArray g = do
  len <- getWord8 >>= \case
    t | Just sz <- is_TAG_fixarray t -> pure sz
    TAG_array16 -> intCast <$> getWord16be
    TAG_array32 -> getWord32be
    _           -> empty
  len' <- fromSizeM "getArray: data exceeds capacity of Vector" len
  V.replicateM len' g

getMap :: Get a -> Get b -> Get (V.Vector (a, b))
getMap k v = do
  len <- getWord8 >>= \case
    t | Just sz <- is_TAG_fixmap t -> pure sz
    TAG_map16 -> intCast <$> getWord16be
    TAG_map32 -> getWord32be
    _         -> empty
  len' <- fromSizeM "getMap: data exceeds capacity of Vector" len
  V.replicateM len' $ (,) <$> k <*> v

getExt :: Get (Int8, S.ByteString)
getExt = getExt' $ \typ len -> do
  len' <- fromSizeM "getExt: data exceeds capacity of ByteString" len
  (,) typ <$> getByteString len'

-- | @since 1.1.0.0
getExt' :: (Int8 -> Word32 -> Get a) -> Get a
getExt' getdat = do
  len <- getWord8 >>= \case
    TAG_fixext1  -> return 1
    TAG_fixext2  -> return 2
    TAG_fixext4  -> return 4
    TAG_fixext8  -> return 8
    TAG_fixext16 -> return 16
    TAG_ext8     -> intCast <$> getWord8
    TAG_ext16    -> intCast <$> getWord16be
    TAG_ext32    -> getWord32be
    _            -> empty
  typ <- getInt8
  getdat typ len

fromSizeM :: String -> Word32 -> Get Int
fromSizeM label sz = maybe (fail label) pure (intCastMaybe sz)
