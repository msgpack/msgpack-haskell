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

putArray :: (a -> Put) -> [a] -> Put
putArray p xs = do
  case length xs of
    len | len <= 15 ->
          putWord8 $ 0x90 .|. fromIntegral len
        | len < 0x10000 ->
          putWord8 0xDC >> putWord16be (fromIntegral len)
        | otherwise ->
          putWord8 0xDD >> putWord32be (fromIntegral len)
  mapM_ p xs

putMap :: (a -> Put) -> (b -> Put) -> [(a, b)] -> Put
putMap p q xs = do
  case length xs of
    len | len <= 15 ->
          putWord8 $ 0x80 .|. fromIntegral len
        | len < 0x10000 ->
          putWord8 0xDE >> putWord16be (fromIntegral len)
        | otherwise ->
          putWord8 0xDF >> putWord32be (fromIntegral len)
  mapM_ (\(a, b) -> p a >> q b ) xs

{-
  -- * Serializable class
  Packable(..),
  -- * Simple function to pack a Haskell value
  pack,

-- | Serializable class
class Packable a where
  -- | Serialize a value
  from :: a -> Builder

-- | Pack Haskell data to MessagePack string.
pack :: Packable a => a -> BL.ByteString
pack = toLazyByteString . from

instance Packable String where
  from = fromString encodeUtf8 B.length fromByteString

instance Packable B.ByteString where
  from = fromString id B.length fromByteString

instance Packable BL.ByteString where
  from = fromString id (fromIntegral . BL.length) fromLazyByteString

instance Packable T.Text where
  from = fromString T.encodeUtf8 B.length fromByteString

instance Packable TL.Text where
  from = fromString TL.encodeUtf8 (fromIntegral . BL.length) fromLazyByteString

fromString :: (s -> t) -> (t -> Int) -> (t -> Builder) -> s -> Builder
fromString cnv lf pf str =

instance Packable a => Packable [a] where
  from = fromArray length (Monoid.mconcat . map from)

instance Packable a => Packable (V.Vector a) where
  from = fromArray V.length (V.foldl (\a b -> a <> from b) Monoid.mempty)

instance (Packable a1, Packable a2) => Packable (a1, a2) where
  from = fromArray (const 2) f where
    f (a1, a2) = from a1 <> from a2

instance (Packable a1, Packable a2, Packable a3) => Packable (a1, a2, a3) where
  from = fromArray (const 3) f where
    f (a1, a2, a3) = from a1 <> from a2 <> from a3

instance (Packable a1, Packable a2, Packable a3, Packable a4) => Packable (a1, a2, a3, a4) where
  from = fromArray (const 4) f where
    f (a1, a2, a3, a4) = from a1 <> from a2 <> from a3 <> from a4

instance (Packable a1, Packable a2, Packable a3, Packable a4, Packable a5) => Packable (a1, a2, a3, a4, a5) where
  from = fromArray (const 5) f where
    f (a1, a2, a3, a4, a5) = from a1 <> from a2 <> from a3 <> from a4 <> from a5

instance (Packable a1, Packable a2, Packable a3, Packable a4, Packable a5, Packable a6) => Packable (a1, a2, a3, a4, a5, a6) where
  from = fromArray (const 6) f where
    f (a1, a2, a3, a4, a5, a6) = from a1 <> from a2 <> from a3 <> from a4 <> from a5 <> from a6

instance (Packable a1, Packable a2, Packable a3, Packable a4, Packable a5, Packable a6, Packable a7) => Packable (a1, a2, a3, a4, a5, a6, a7) where
  from = fromArray (const 7) f where
    f (a1, a2, a3, a4, a5, a6, a7) = from a1 <> from a2 <> from a3 <> from a4 <> from a5 <> from a6 <> from a7

instance (Packable a1, Packable a2, Packable a3, Packable a4, Packable a5, Packable a6, Packable a7, Packable a8) => Packable (a1, a2, a3, a4, a5, a6, a7, a8) where
  from = fromArray (const 8) f where
    f (a1, a2, a3, a4, a5, a6, a7, a8) = from a1 <> from a2 <> from a3 <> from a4 <> from a5 <> from a6 <> from a7 <> from a8

instance (Packable a1, Packable a2, Packable a3, Packable a4, Packable a5, Packable a6, Packable a7, Packable a8, Packable a9) => Packable (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  from = fromArray (const 9) f where
    f (a1, a2, a3, a4, a5, a6, a7, a8, a9) = from a1 <> from a2 <> from a3 <> from a4 <> from a5 <> from a6 <> from a7 <> from a8 <> from a9

instance (Packable k, Packable v) => Packable (Assoc [(k,v)]) where
  from = fromMap length (Monoid.mconcat . map fromPair) . unAssoc

instance (Packable k, Packable v) => Packable (Assoc (V.Vector (k,v))) where
  from = fromMap V.length (V.foldl (\a b -> a <> fromPair b) Monoid.mempty) . unAssoc

instance (Packable k, Packable v) => Packable (M.Map k v) where
  from = fromMap M.size (Monoid.mconcat . map fromPair . M.toList)

instance Packable v => Packable (IM.IntMap v) where
  from = fromMap IM.size (Monoid.mconcat . map fromPair . IM.toList)

instance (Packable k, Packable v) => Packable (HM.HashMap k v) where
  from = fromMap HM.size (Monoid.mconcat . map fromPair . HM.toList)

instance Packable a => Packable (Maybe a) where
  from Nothing = from ()
  from (Just a) = from a
-}
