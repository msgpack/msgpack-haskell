--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Pack
-- Copyright : (c) Hideyuki Tanaka, 2009-2015
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- MessagePack Serializer using @Data.Binary@
--
--------------------------------------------------------------------

module Data.MessagePack.Pack (
  putNil, putBool, putInt, putFloat, putDouble,
  putRAW, putArray, putMap,
  ) where

import           Data.Binary
import           Data.Binary.IEEE754
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString     as S
import qualified Data.Vector         as V

putNil :: Put
putNil = putWord8 0xC0

putBool :: Bool -> Put
putBool False = putWord8 0xC2
putBool True  = putWord8 0xC3

putInt :: Int -> Put
putInt n
  | -32 <= n && n <= 127 =
    putWord8 $ fromIntegral n
  | 0 <= n && n < 0x100 =
    putWord8 0xCC >> putWord8     (fromIntegral n)
  | 0 <= n && n < 0x10000 =
    putWord8 0xCD >> putWord16be  (fromIntegral n)
  | 0 <= n && n < 0x100000000 =
    putWord8 0xCE >> putWord32be  (fromIntegral n)
  | 0 <= n =
    putWord8 0xCF >> putWord64be  (fromIntegral n)
  | -0x80 <= n =
    putWord8 0xD0 >> putWord8     (fromIntegral n)
  | -0x8000 <= n =
    putWord8 0xD1 >> putWord16be  (fromIntegral n)
  | -0x80000000  <= n =
    putWord8 0xD2 >> putWord32be  (fromIntegral n)
  | otherwise =
    putWord8 0xD3 >> putWord64be (fromIntegral n)

putFloat :: Float -> Put
putFloat f = do
  putWord8 0xCB
  putFloat32be f

putDouble :: Double -> Put
putDouble d = do
  putWord8 0xCB
  putFloat64be d

putRAW :: S.ByteString -> Put
putRAW bs = do
  case S.length bs of
    len | len <= 31 ->
          putWord8 $ 0xA0 .|. fromIntegral len
        | len < 0x10000 ->
          putWord8 0xDA >> putWord16be (fromIntegral len)
        | otherwise ->
          putWord8 0xDB >> putWord32be (fromIntegral len)
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
