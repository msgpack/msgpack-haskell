--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Put
-- Copyright : © Hideyuki Tanaka 2009-2015
--           , © Herbert Valerio Riedel 2019
-- License   : BSD3
--
-- MessagePack Serializer using "Data.Binary".
--
--------------------------------------------------------------------

module Data.MessagePack.Put (
  putNil, putBool, putFloat, putDouble,
  putInt, putWord, putInt64, putWord64,
  putStr, putBin, putArray, putMap, putExt,
  ) where

import           Data.Binary
import           Data.Binary.IEEE754 (putFloat32be, putFloat64be)
import           Data.Binary.Put     (putByteString, putWord16be, putWord32be,
                                      putWord64be, putWord8)
import           Data.Bits
import qualified Data.ByteString     as S
import           Data.Int
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Vector         as V

import           Prelude             hiding (putStr)

putNil :: Put
putNil = putWord8 0xC0

putBool :: Bool -> Put
putBool False = putWord8 0xC2
putBool True  = putWord8 0xC3

putInt :: Int -> Put
putInt n = putInt64 (fromIntegral n)

-- | @since 1.0.1.0
putWord :: Word -> Put
putWord n = putWord64 (fromIntegral n)

-- | @since 1.0.1.0
putInt64 :: Int64 -> Put
putInt64 n
    -- positive fixnum stores 7-bit positive integer
    -- negative fixnum stores 5-bit negative integer
  | -32 <= n && n <= 127 = putWord8 $ fromIntegral n

    -- unsigned int encoding
  | n >= 0 = putWord64 (fromIntegral n)

    -- signed int encoding
  | -0x80       <= n = putWord8 0xD0 >> putWord8     (fromIntegral n)
  | -0x8000     <= n = putWord8 0xD1 >> putWord16be  (fromIntegral n)
  | -0x80000000 <= n = putWord8 0xD2 >> putWord32be  (fromIntegral n)
  | otherwise        = putWord8 0xD3 >> putWord64be  (fromIntegral n)

-- | @since 1.0.1.0
putWord64 :: Word64 -> Put
putWord64 n
    -- positive fixnum stores 7-bit positive integer
  | n < 0x80        = putWord8 $ fromIntegral n

    -- unsigned int encoding
  | n < 0x100       = putWord8 0xCC >> putWord8     (fromIntegral n)
  | n < 0x10000     = putWord8 0xCD >> putWord16be  (fromIntegral n)
  | n < 0x100000000 = putWord8 0xCE >> putWord32be  (fromIntegral n)
  | otherwise       = putWord8 0xCF >> putWord64be  (fromIntegral n)

putFloat :: Float -> Put
putFloat f = do
  putWord8 0xCA
  putFloat32be f

putDouble :: Double -> Put
putDouble d = do
  putWord8 0xCB
  putFloat64be d

putStr :: T.Text -> Put
putStr t = do
  let bs = T.encodeUtf8 t
  case S.length bs of
    len | len <= 31 ->
          putWord8 $ 0xA0 .|. fromIntegral len
        | len < 0x100 ->
          putWord8 0xD9 >> putWord8    (fromIntegral len)
        | len < 0x10000 ->
          putWord8 0xDA >> putWord16be (fromIntegral len)
        | otherwise ->
          putWord8 0xDB >> putWord32be (fromIntegral len)
  putByteString bs

putBin :: S.ByteString -> Put
putBin bs = do
  case S.length bs of
    len | len < 0x100 ->
          putWord8 0xC4 >> putWord8    (fromIntegral len)
        | len < 0x10000 ->
          putWord8 0xC5 >> putWord16be (fromIntegral len)
        | otherwise ->
          putWord8 0xC6 >> putWord32be (fromIntegral len)
  putByteString bs

putArray :: (a -> Put) -> V.Vector a -> Put
putArray p xs = do
  case V.length xs of
    len | len <= 15 ->
          putWord8 $ 0x90 .|. fromIntegral len
        | len < 0x10000 ->
          putWord8 0xDC >> putWord16be (fromIntegral len)
        | otherwise ->
          putWord8 0xDD >> putWord32be (fromIntegral len)
  V.mapM_ p xs

putMap :: (a -> Put) -> (b -> Put) -> V.Vector (a, b) -> Put
putMap p q xs = do
  case V.length xs of
    len | len <= 15 ->
          putWord8 $ 0x80 .|. fromIntegral len
        | len < 0x10000 ->
          putWord8 0xDE >> putWord16be (fromIntegral len)
        | otherwise ->
          putWord8 0xDF >> putWord32be (fromIntegral len)
  V.mapM_ (\(a, b) -> p a >> q b ) xs

putExt :: Word8 -> S.ByteString -> Put
putExt typ dat = do
  case S.length dat of
    1  -> putWord8 0xD4
    2  -> putWord8 0xD5
    4  -> putWord8 0xD6
    8  -> putWord8 0xD7
    16 -> putWord8 0xD8
    len | len < 0x100   -> putWord8 0xC7 >> putWord8    (fromIntegral len)
        | len < 0x10000 -> putWord8 0xC8 >> putWord16be (fromIntegral len)
        | otherwise     -> putWord8 0xC9 >> putWord32be (fromIntegral len)
  putWord8 typ
  putByteString dat
