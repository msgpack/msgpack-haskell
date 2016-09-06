{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Object
-- Copyright : (c) Hideyuki Tanaka, 2009-2015
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- MessagePack object definition
--
--------------------------------------------------------------------

module Data.MessagePack.Class
  ( MessagePack (..)
  , GMessagePack (..)
  ) where

import           Control.Applicative     (Applicative, (<$>), (<*>))
import           Control.Arrow           ((***))
import qualified Data.ByteString         as S
import qualified Data.ByteString.Lazy    as L
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Int                (Int16, Int32, Int64, Int8)
import qualified Data.IntMap.Strict      as IntMap
import qualified Data.Map                as Map
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import           Data.Word               (Word, Word16, Word32, Word64, Word8)
import           GHC.Generics

import           Data.MessagePack.Assoc
import           Data.MessagePack.Object


-- Generic serialisation.

class GMessagePack f where
  gToObject   :: f a -> Object
  gFromObject :: (Applicative m, Monad m) => Object -> m (f a)


class MessagePack a where
  toObject   :: a -> Object
  fromObject :: (Applicative m, Monad m) => Object -> m a

  default toObject :: (Generic a, GMessagePack (Rep a))
                   => a -> Object
  toObject = genericToObject
  default fromObject :: ( Applicative m, Monad m
                        , Generic a, GMessagePack (Rep a))
                     => Object -> m a
  fromObject = genericFromObject


genericToObject :: (Generic a, GMessagePack (Rep a))
                => a -> Object
genericToObject = gToObject . from

genericFromObject :: ( Applicative m, Monad m
                     , Generic a, GMessagePack (Rep a))
                  => Object -> m a
genericFromObject x = to <$> gFromObject x


-- Instances for integral types (Int etc.).

toInt :: Integral a => a -> Int64
toInt = fromIntegral

fromInt :: Integral a => Int64 -> a
fromInt = fromIntegral

instance MessagePack Int64 where
  toObject = ObjectInt
  fromObject = \case
    ObjectInt n -> return n
    _           -> fail "invalid encoding for integer type"

instance MessagePack Int    where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Int8   where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Int16  where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Int32  where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }

instance MessagePack Word   where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Word8  where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Word16 where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Word32 where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }
instance MessagePack Word64 where { toObject = toObject . toInt; fromObject o = fromInt <$> fromObject o }


-- Core instances.

instance MessagePack Object where
  toObject = id
  fromObject = return

instance MessagePack () where
  toObject _ = ObjectArray []
  fromObject = \case
    ObjectArray [] -> return ()
    _              -> fail "invalid encoding for ()"

instance MessagePack Bool where
  toObject = ObjectBool
  fromObject = \case
    ObjectBool b -> return b
    _            -> fail "invalid encoding for Bool"

instance MessagePack Float where
  toObject = ObjectFloat
  fromObject = \case
    ObjectInt    n -> return $ fromIntegral n
    ObjectFloat  f -> return f
    ObjectDouble d -> return $ realToFrac d
    _              -> fail "invalid encoding for Float"

instance MessagePack Double where
  toObject = ObjectDouble
  fromObject = \case
    ObjectInt    n -> return $ fromIntegral n
    ObjectFloat  f -> return $ realToFrac f
    ObjectDouble d -> return d
    _              -> fail "invalid encoding for Double"

-- Because of overlapping instance, this must be above [a].
-- IncoherentInstances and TypeSynonymInstances are required for this to work.
instance MessagePack String where
  toObject = toObject . T.pack
  fromObject obj = T.unpack <$> fromObject obj


-- Instances for nullable types.

instance MessagePack a => MessagePack (Maybe a) where
  toObject = \case
    Just a  -> toObject a
    Nothing -> ObjectNil

  fromObject = \case
    ObjectNil -> return Nothing
    obj       -> Just <$> fromObject obj


-- Instances for binary and UTF-8 encoded string.

instance MessagePack S.ByteString where
  toObject = ObjectBin
  fromObject = \case
    ObjectBin r -> return r
    _           -> fail "invalid encoding for ByteString"

instance MessagePack L.ByteString where
  toObject = ObjectBin . L.toStrict
  fromObject obj = L.fromStrict <$> fromObject obj

instance MessagePack T.Text where
  toObject = ObjectStr
  fromObject = \case
    ObjectStr s -> return s
    _           -> fail "invalid encoding for Text"

instance MessagePack LT.Text where
  toObject = toObject . LT.toStrict
  fromObject obj = LT.fromStrict <$> fromObject obj


-- Instances for array-like data structures.

instance MessagePack a => MessagePack [a] where
  toObject = ObjectArray . map toObject
  fromObject = \case
    ObjectArray xs -> mapM fromObject xs
    _              -> fail "invalid encoding for list"


-- Instances for map-like data structures.

instance (MessagePack a, MessagePack b) => MessagePack (Assoc [(a, b)]) where
  toObject (Assoc xs) = ObjectMap $ map (toObject *** toObject) xs
  fromObject = \case
    ObjectMap xs ->
      Assoc <$> mapM (\(k, v) -> (,) <$> fromObject k <*> fromObject v) xs
    _ ->
      fail "invalid encoding for Assoc"

instance (MessagePack k, MessagePack v, Ord k) => MessagePack (Map.Map k v) where
  toObject = toObject . Assoc . Map.toList
  fromObject obj = Map.fromList . unAssoc <$> fromObject obj

instance MessagePack v => MessagePack (IntMap.IntMap v) where
  toObject = toObject . Assoc . IntMap.toList
  fromObject obj = IntMap.fromList . unAssoc <$> fromObject obj

instance (MessagePack k, MessagePack v, Hashable k, Eq k) => MessagePack (HashMap.HashMap k v) where
  toObject = toObject . Assoc . HashMap.toList
  fromObject obj = HashMap.fromList . unAssoc <$> fromObject obj


-- Instances for various tuple arities.

instance (MessagePack a1, MessagePack a2) => MessagePack (a1, a2) where
  toObject (a1, a2) = ObjectArray [toObject a1, toObject a2]
  fromObject (ObjectArray [a1, a2]) = (,) <$> fromObject a1 <*> fromObject a2
  fromObject _                      = fail "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3) => MessagePack (a1, a2, a3) where
  toObject (a1, a2, a3) = ObjectArray [toObject a1, toObject a2, toObject a3]
  fromObject (ObjectArray [a1, a2, a3]) = (,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3
  fromObject _ = fail "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4) => MessagePack (a1, a2, a3, a4) where
  toObject (a1, a2, a3, a4) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4]
  fromObject (ObjectArray [a1, a2, a3, a4]) = (,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4
  fromObject _ = fail "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5) => MessagePack (a1, a2, a3, a4, a5) where
  toObject (a1, a2, a3, a4, a5) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5]
  fromObject (ObjectArray [a1, a2, a3, a4, a5]) = (,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5
  fromObject _ = fail "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6) => MessagePack (a1, a2, a3, a4, a5, a6) where
  toObject (a1, a2, a3, a4, a5, a6) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6]) = (,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6
  fromObject _ = fail "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7) => MessagePack (a1, a2, a3, a4, a5, a6, a7) where
  toObject (a1, a2, a3, a4, a5, a6, a7) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7]) = (,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7
  fromObject _ = fail "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7, MessagePack a8) => MessagePack (a1, a2, a3, a4, a5, a6, a7, a8) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7, a8]) = (,,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7 <*> fromObject a8
  fromObject _ = fail "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7, MessagePack a8, MessagePack a9) => MessagePack (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8, a9) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8, toObject a9]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7, a8, a9]) = (,,,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7 <*> fromObject a8 <*> fromObject a9
  fromObject _ = fail "invalid encoding for tuple"
