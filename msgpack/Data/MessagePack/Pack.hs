{-# LANGUAGE FlexibleInstances, IncoherentInstances, TypeSynonymInstances #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Pack
-- Copyright : (c) Hideyuki Tanaka, 2009-2011
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- MessagePack Serializer using @Data.Binary.Pack@
--
--------------------------------------------------------------------

module Data.MessagePack.Pack (
  -- * Serializable class
  Packable(..),
  -- * Simple function to pack a Haskell value
  pack,
  ) where

import Blaze.ByteString.Builder
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector as V
import Foreign

import Data.MessagePack.Assoc
import Data.MessagePack.Internal.Utf8

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- | Serializable class
class Packable a where
  -- | Serialize a value
  from :: a -> Builder

-- | Pack Haskell data to MessagePack string.
pack :: Packable a => a -> BL.ByteString
pack = toLazyByteString . from

instance Packable Int where
  from n =
    case n of
      _ | n >= 0 && n <= 127 ->
        fromWord8 $ fromIntegral n
      _ | n >= -32 && n <= -1 ->
        fromWord8 $ fromIntegral n
      _ | n >= 0 && n < 0x100 ->
        fromWord8 0xCC <>
        fromWord8 (fromIntegral n)
      _ | n >= 0 && n < 0x10000 ->
        fromWord8 0xCD <>
        fromWord16be (fromIntegral n)
      _ | n >= 0 && n < 0x100000000 ->
        fromWord8 0xCE <>
        fromWord32be (fromIntegral n)
      _ | n >= 0 ->
        fromWord8 0xCF <>
        fromWord64be (fromIntegral n)
      _ | n >= -0x80 ->
        fromWord8 0xD0 <>
        fromWord8 (fromIntegral n)
      _ | n >= -0x8000 ->
        fromWord8 0xD1 <>
        fromWord16be (fromIntegral n)
      _ | n >= -0x80000000 ->
        fromWord8 0xD2 <>
        fromWord32be (fromIntegral n)
      _ ->
        fromWord8 0xD3 <>
        fromWord64be (fromIntegral n)
      
instance Packable () where
  from _ = 
    fromWord8 0xC0

instance Packable Bool where
  from True  = fromWord8 0xC3
  from False = fromWord8 0xC2

instance Packable Float where
  from f =
    fromWord8 0xCB <>
    fromWord32be (cast f)

instance Packable Double where
  from d =
    fromWord8 0xCB <>
    fromWord64be (cast d)

cast :: (Storable a, Storable b) => a -> b
cast v = unsafePerformIO $ with v $ peek . castPtr

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
  let bs = cnv str in
  case lf bs of
    len | len <= 31 ->
      fromWord8 $ 0xA0 .|. fromIntegral len
    len | len < 0x10000 ->
      fromWord8 0xDA <>
      fromWord16be (fromIntegral len)
    len ->
      fromWord8 0xDB <>
      fromWord32be (fromIntegral len)
  <> pf bs

instance Packable a => Packable [a] where
  from = fromArray length (mconcat . map from)

instance Packable a => Packable (V.Vector a) where
  from = fromArray V.length (V.foldl (\a b -> a <> from b) mempty)

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

fromArray :: (a -> Int) -> (a -> Builder) -> a -> Builder
fromArray lf pf arr = do
  case lf arr of
    len | len <= 15 ->
      fromWord8 $ 0x90 .|. fromIntegral len
    len | len < 0x10000 ->
      fromWord8 0xDC <>
      fromWord16be (fromIntegral len)
    len ->
      fromWord8 0xDD <>
      fromWord32be (fromIntegral len)
  <> pf arr

instance (Packable k, Packable v) => Packable (Assoc [(k,v)]) where
  from = fromMap length (mconcat . map fromPair) . unAssoc

instance (Packable k, Packable v) => Packable (Assoc (V.Vector (k,v))) where
  from = fromMap V.length (V.foldl (\a b -> a <> fromPair b) mempty) . unAssoc

instance (Packable k, Packable v) => Packable (M.Map k v) where
  from = fromMap M.size (mconcat . map fromPair . M.toList)

instance Packable v => Packable (IM.IntMap v) where
  from = fromMap IM.size (mconcat . map fromPair . IM.toList)

instance (Packable k, Packable v) => Packable (HM.HashMap k v) where
  from = fromMap HM.size (mconcat . map fromPair . HM.toList)

fromPair :: (Packable a, Packable b) => (a, b) -> Builder
fromPair (a, b) = from a <> from b

fromMap :: (a -> Int) -> (a -> Builder) -> a -> Builder
fromMap lf pf m =
  case lf m of
    len | len <= 15 ->
      fromWord8 $ 0x80 .|. fromIntegral len
    len | len < 0x10000 ->
      fromWord8 0xDE <>
      fromWord16be (fromIntegral len)
    len ->
      fromWord8 0xDF <>
      fromWord32be (fromIntegral len)
  <> pf m

instance Packable a => Packable (Maybe a) where
  from Nothing = from ()
  from (Just a) = from a
