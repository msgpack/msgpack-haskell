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
  ( getNil, tryNil
  , getBool, tryBool

  , getFloat, tryFloat
  , getDouble, tryDouble

  , getStr, tryStr
  , getBin, tryBin

  , getArray, tryArray
  , getMap, tryMap

  , getExt, tryExt
  , getExt', tryExt'
  ) where

import           Compat.Prelude

import qualified Data.ByteString       as S
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Vector           as V

import           Compat.Binary
import           Data.MessagePack.Tags

mkGet :: (Word8 -> t -> Get a -> Get b) -> t -> String -> Get b
mkGet tryT f n = do { tag <- getWord8; tryT tag f empty } <|> fail n

getNil :: Get ()
getNil = mkGet tryNil id "expected MessagePack nil"

getBool :: Get Bool
getBool = mkGet tryBool id "expected MessagePack bool"

getFloat :: Get Float
getFloat = mkGet tryFloat id "expected MessagePack float32"

getDouble :: Get Double
getDouble = mkGet tryDouble id "expected MessagePack float64"

getStr :: Get T.Text
getStr = mkGet tryStr id "expected MessagePack str"

getBin :: Get S.ByteString
getBin = mkGet tryBin id "expected MessagePack bin"

getArray :: Get a -> Get (V.Vector a)
getArray g = mkGet (tryArray g) id "expected MessagePack array"

getMap :: Get a -> Get b -> Get (V.Vector (a, b))
getMap k v = mkGet (tryMap k v) id "Map"

getExt :: Get (Int8, S.ByteString)
getExt = mkGet tryExt id "expected MessagePack ext"

-- | @since 1.1.0.0
getExt' :: (Int8 -> Word32 -> Get a) -> Get a
getExt' getdat = mkGet (tryExt' getdat) id "expected MessagePack ext"

----------------------------------------------------------------------------
-- primitives that take a tag as first argument

{-# INLINE tryNil #-}
tryNil :: Word8 -> (() -> a) -> Get a -> Get a
tryNil tag f cont = case tag of
  TAG_nil -> pure $! f ()
  _       -> cont

{-# INLINE tryBool #-}
tryBool :: Word8 -> (Bool -> a) -> Get a -> Get a
tryBool tag f cont = case tag of
  TAG_false -> pure $! f False
  TAG_true  -> pure $! f True
  _         -> cont

{-# INLINE tryFloat #-}
tryFloat :: Word8 -> (Float -> a) -> Get a -> Get a
tryFloat tag f cont = case tag of
  TAG_float32 -> f <$> getFloat32be
  _           -> cont

{-# INLINE tryDouble #-}
tryDouble :: Word8 -> (Double -> a) -> Get a -> Get a
tryDouble tag f cont = case tag of
  TAG_float64 -> f <$> getFloat64be
  _           -> cont

{-# INLINE tryStr #-}
tryStr :: Word8 -> (T.Text -> a) -> Get a -> Get a
tryStr tag f cont = case tag of
    t | Just sz <- is_TAG_fixstr t -> cont' sz
    TAG_str8                       -> cont' . intCast =<< getWord8
    TAG_str16                      -> cont' . intCast =<< getWord16be
    TAG_str32                      -> cont' =<< getWord32be
    _                              -> cont
  where
    cont' len = do
      len' <- fromSizeM "getStr: data exceeds capacity of ByteString/Text" len
      bs <- getByteString len'
      case T.decodeUtf8' bs of
        Left _  -> fail "getStr: invalid UTF-8 encoding"
        Right v -> pure $! f v

{-# INLINE tryBin #-}
tryBin :: Word8 -> (S.ByteString -> a) -> Get a -> Get a
tryBin tag f cont = case tag of
    TAG_bin8  -> cont' . intCast =<< getWord8
    TAG_bin16 -> cont' . intCast =<< getWord16be
    TAG_bin32 -> cont' =<< getWord32be
    _         -> cont
  where
    cont' len = do
      len' <- fromSizeM "getBin: data exceeds capacity of ByteString" len
      f <$> getByteString len'

{-# INLINE tryArray #-}
tryArray :: Get b -> Word8 -> (V.Vector b -> a) -> Get a -> Get a
tryArray g tag f cont = case tag of
    t | Just sz <- is_TAG_fixarray t -> cont' sz
    TAG_array16                      -> cont' . intCast =<< getWord16be
    TAG_array32                      -> cont' =<< getWord32be
    _                                -> cont
  where
    cont' len = do
      len' <- fromSizeM "getArray: data exceeds capacity of Vector" len
      f <$> V.replicateM len' g

{-# INLINE tryMap #-}
tryMap :: Get k -> Get v -> Word8 -> (V.Vector (k,v) -> a) -> Get a -> Get a
tryMap k v tag f cont = case tag of
    t | Just sz <- is_TAG_fixmap t -> cont' sz
    TAG_map16                      -> cont' . intCast =<< getWord16be
    TAG_map32                      -> cont' =<< getWord32be
    _                              -> cont
  where
    cont' len = do
      len' <- fromSizeM "getMap: data exceeds capacity of Vector" len
      f <$> V.replicateM len' ((,) <$> k <*> v)

{-# INLINE tryExt #-}
tryExt :: Word8 -> ((Int8,S.ByteString) -> a) -> Get a -> Get a
tryExt tag f cont = tryExt' go tag f cont
  where
    go :: Int8 -> Word32 -> Get (Int8,S.ByteString)
    go typ len = do
      len' <- fromSizeM "getExt: data exceeds capacity of ByteString" len
      (,) typ <$> getByteString len'


{-# INLINE tryExt' #-}
tryExt' :: (Int8 -> Word32 -> Get b) -> Word8 -> (b -> a) -> Get a -> Get a
tryExt' g tag f cont = case tag of
    TAG_fixext1  -> cont' 1
    TAG_fixext2  -> cont' 2
    TAG_fixext4  -> cont' 4
    TAG_fixext8  -> cont' 8
    TAG_fixext16 -> cont' 16
    TAG_ext8     -> cont' . intCast =<< getWord8
    TAG_ext16    -> cont' . intCast =<< getWord16be
    TAG_ext32    -> cont' =<< getWord32be
    _            -> cont

  where
    cont' len = do
      typ <- getInt8
      f <$> g typ len


fromSizeM :: String -> Word32 -> Get Int
fromSizeM label sz = maybe (fail label) pure (intCastMaybe sz)
