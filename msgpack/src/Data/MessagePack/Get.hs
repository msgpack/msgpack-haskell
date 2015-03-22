{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Get
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

module Data.MessagePack.Get(
  getNil, getBool, getInt, getFloat, getDouble,
  getRAW, getArray, getMap,
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.IEEE754
import           Data.Bits
import qualified Data.ByteString     as S
import           Data.Int
import qualified Data.Vector         as V

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

getArray :: Get a -> Get (V.Vector a)
getArray g = do
  len <- getWord8 >>= \case
    t | t .&. 0xF0 == 0x90 ->
      return $ fromIntegral $ t .&. 0x0F
    0xDC -> fromIntegral <$> getWord16be
    0xDD -> fromIntegral <$> getWord32be
    _    -> empty
  V.replicateM len g

getMap :: Get a -> Get b -> Get (V.Vector (a, b))
getMap k v = do
  len <- getWord8 >>= \case
    t | t .&. 0xF0 == 0x80 ->
      return $ fromIntegral $ t .&. 0x0F
    0xDE -> fromIntegral <$> getWord16be
    0xDF -> fromIntegral <$> getWord32be
    _    -> empty
  V.replicateM len $ (,) <$> k <*> v

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
