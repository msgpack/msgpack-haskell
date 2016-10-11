{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators        #-}
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

module Data.MessagePack.Object(
  -- * MessagePack Object
  Object(..),

  -- * MessagePack Serializable Types
  MessagePack(..),
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad
import           Data.Binary
import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L
import           Data.Hashable
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.IntMap.Strict     as IntMap
import qualified Data.Map               as Map
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import           Data.Typeable
import qualified Data.Vector            as V
import           Data.Void
import           GHC.Generics       hiding (from, to)
import qualified GHC.Generics           as G (from, to)

import           Data.MessagePack.Assoc
import           Data.MessagePack.Get
import           Data.MessagePack.Put

import           Prelude                hiding (putStr)


-- | Object Representation of MessagePack data.
data Object
  = ObjectNil
    -- ^ represents nil
  | ObjectBool                  !Bool
    -- ^ represents true or false
  | ObjectInt    {-# UNPACK #-} !Int
    -- ^ represents an integer
  | ObjectFloat  {-# UNPACK #-} !Float
    -- ^ represents a floating point number
  | ObjectDouble {-# UNPACK #-} !Double
    -- ^ represents a floating point number
  | ObjectStr                   !T.Text
    -- ^ extending Raw type represents a UTF-8 string
  | ObjectBin                   !S.ByteString
    -- ^ extending Raw type represents a byte array
  | ObjectArray                 !(V.Vector Object)
    -- ^ represents a sequence of objects
  | ObjectMap                   !(V.Vector (Object, Object))
    -- ^ represents key-value pairs of objects
  | ObjectExt    {-# UNPACK #-} !Word8 !S.ByteString
    -- ^ represents a tuple of an integer and a byte array where
    -- the integer represents type information and the byte array represents data.
  deriving (Show, Eq, Ord, Typeable)

instance NFData Object where
  rnf obj = case obj of
    ObjectArray a -> rnf a
    ObjectMap   m -> rnf m
    _             -> ()

getObject :: Get Object
getObject =
      ObjectNil    <$  getNil
  <|> ObjectBool   <$> getBool
  <|> ObjectInt    <$> getInt
  <|> ObjectFloat  <$> getFloat
  <|> ObjectDouble <$> getDouble
  <|> ObjectStr    <$> getStr
  <|> ObjectBin    <$> getBin
  <|> ObjectArray  <$> getArray getObject
  <|> ObjectMap    <$> getMap getObject getObject
  <|> uncurry ObjectExt <$> getExt

putObject :: Object -> Put
putObject = \case
  ObjectNil      -> putNil
  ObjectBool   b -> putBool b
  ObjectInt    n -> putInt n
  ObjectFloat  f -> putFloat f
  ObjectDouble d -> putDouble d
  ObjectStr    t -> putStr t
  ObjectBin    b -> putBin b
  ObjectArray  a -> putArray putObject a
  ObjectMap    m -> putMap putObject putObject m
  ObjectExt  b r -> putExt b r

instance Binary Object where
  get = getObject
  put = putObject

class MessagePack a where
  toObject   :: a -> Object
  default toObject
    :: (Generic a, GenericMessagePack (Rep a))
    => a -> Object
  toObject = genericToObject . G.from

  fromObject :: Object -> Maybe a
  default fromObject
    :: (Generic a, GenericMessagePack (Rep a))
    => Object -> Maybe a
  fromObject = fmap G.to . genericFromObject

-- core instances

instance MessagePack Object where
  toObject = id
  fromObject = Just

instance MessagePack () where
  toObject _ = ObjectArray V.empty
  fromObject = \case
    ObjectArray v ->
        if V.null v
            then Just ()
            else Nothing
    _ -> Nothing

instance MessagePack Int where
  toObject = ObjectInt
  fromObject = \case
    ObjectInt n -> Just n
    _           -> Nothing

instance MessagePack Bool where
  toObject = ObjectBool
  fromObject = \case
    ObjectBool b -> Just b
    _            -> Nothing

instance MessagePack Float where
  toObject = ObjectFloat
  fromObject = \case
    ObjectInt    n -> Just $ fromIntegral n
    ObjectFloat  f -> Just f
    ObjectDouble d -> Just $ realToFrac d
    _              -> Nothing

instance MessagePack Double where
  toObject = ObjectDouble
  fromObject = \case
    ObjectInt    n -> Just $ fromIntegral n
    ObjectFloat  f -> Just $ realToFrac f
    ObjectDouble d -> Just d
    _              -> Nothing

instance MessagePack S.ByteString where
  toObject = ObjectBin
  fromObject = \case
    ObjectBin r -> Just r
    _           -> Nothing

-- Because of overlapping instance, this must be above [a]
instance MessagePack String where
  toObject = toObject . T.pack
  fromObject obj = T.unpack <$> fromObject obj

instance MessagePack a => MessagePack (V.Vector a) where
  toObject = ObjectArray . V.map toObject
  fromObject = \case
    ObjectArray xs -> V.mapM fromObject xs
    _              -> Nothing

instance (MessagePack a, MessagePack b) => MessagePack (Assoc (V.Vector (a, b))) where
  toObject (Assoc xs) = ObjectMap $ V.map (toObject *** toObject) xs
  fromObject = \case
    ObjectMap xs ->
      Assoc <$> V.mapM (\(k, v) -> (,) <$> fromObject k <*> fromObject v) xs
    _ ->
      Nothing

-- util instances

instance MessagePack Void

instance MessagePack a => MessagePack (Maybe a)

instance (MessagePack a, MessagePack b) => MessagePack (Either a b)

-- UTF8 string like

instance MessagePack L.ByteString where
  toObject = ObjectBin . L.toStrict
  fromObject obj = L.fromStrict <$> fromObject obj

instance MessagePack T.Text where
  toObject = ObjectStr
  fromObject = \case
    ObjectStr s -> Just s
    _           -> Nothing

instance MessagePack LT.Text where
  toObject = toObject . LT.toStrict
  fromObject obj = LT.fromStrict <$> fromObject obj

-- array like

instance MessagePack a => MessagePack [a] where
  toObject = toObject . V.fromList
  fromObject obj = V.toList <$> fromObject obj

-- map like

instance (MessagePack k, MessagePack v) => MessagePack (Assoc [(k, v)]) where
  toObject = toObject . Assoc . V.fromList . unAssoc
  fromObject obj = Assoc . V.toList . unAssoc <$> fromObject obj

instance (MessagePack k, MessagePack v, Ord k) => MessagePack (Map.Map k v) where
  toObject = toObject . Assoc . Map.toList
  fromObject obj = Map.fromList . unAssoc <$> fromObject obj

instance MessagePack v => MessagePack (IntMap.IntMap v) where
  toObject = toObject . Assoc . IntMap.toList
  fromObject obj = IntMap.fromList . unAssoc <$> fromObject obj

instance (MessagePack k, MessagePack v, Hashable k, Eq k) => MessagePack (HashMap.HashMap k v) where
  toObject = toObject . Assoc . HashMap.toList
  fromObject obj = HashMap.fromList . unAssoc <$> fromObject obj

-- tuples (manual definition uses slightly less bytes to serialize)

instance (MessagePack a1, MessagePack a2) => MessagePack (a1, a2) where
  toObject (a1, a2) = ObjectArray [toObject a1, toObject a2]
  fromObject (ObjectArray [a1, a2]) = (,) <$> fromObject a1 <*> fromObject a2
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3) => MessagePack (a1, a2, a3) where
  toObject (a1, a2, a3) = ObjectArray [toObject a1, toObject a2, toObject a3]
  fromObject (ObjectArray [a1, a2, a3]) = (,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4) => MessagePack (a1, a2, a3, a4) where
  toObject (a1, a2, a3, a4) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4]
  fromObject (ObjectArray [a1, a2, a3, a4]) = (,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5) => MessagePack (a1, a2, a3, a4, a5) where
  toObject (a1, a2, a3, a4, a5) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5]
  fromObject (ObjectArray [a1, a2, a3, a4, a5]) = (,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6) => MessagePack (a1, a2, a3, a4, a5, a6) where
  toObject (a1, a2, a3, a4, a5, a6) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6]) = (,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7) => MessagePack (a1, a2, a3, a4, a5, a6, a7) where
  toObject (a1, a2, a3, a4, a5, a6, a7) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7]) = (,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7, MessagePack a8) => MessagePack (a1, a2, a3, a4, a5, a6, a7, a8) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7, a8]) = (,,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7 <*> fromObject a8
  fromObject _ = Nothing

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7, MessagePack a8, MessagePack a9) => MessagePack (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8, a9) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8, toObject a9]
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7, a8, a9]) = (,,,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7 <*> fromObject a8 <*> fromObject a9
  fromObject _ = Nothing

-- generics

class GenericMessagePack f where
  genericToObject :: f a -> Object
  genericFromObject :: Object -> Maybe (f a)

instance GenericMessagePack V1 where
  genericToObject = \case {}
  genericFromObject _ = Nothing

instance GenericMessagePack U1 where
  genericToObject U1 = ObjectNil
  genericFromObject ObjectNil = Just U1
  genericFromObject _         = Nothing

instance GenericMessagePack a => GenericMessagePack (M1 i c a) where
  genericToObject (M1 x) = genericToObject x
  genericFromObject o = M1 <$> genericFromObject o

instance MessagePack a => GenericMessagePack (K1 i a) where
  genericToObject (K1 x) = toObject x
  genericFromObject o = K1 <$> fromObject o

-- TODO: next 2 instances could be optimized
instance (GenericMessagePack a, GenericMessagePack b)
       => GenericMessagePack (a :+: b) where
  genericToObject (L1 x) = toObject (False, genericToObject x)
  genericToObject (R1 x) = toObject (True , genericToObject x)
  genericFromObject o = case fromObject o of
    Just (False, o') -> L1 <$> genericFromObject o'
    Just (True , o') -> R1 <$> genericFromObject o'
    _                -> Nothing

instance (GenericMessagePack a, GenericMessagePack b)
       => GenericMessagePack (a :*: b) where
  genericToObject (a :*: b) = toObject (genericToObject a, genericToObject b)
  genericFromObject o = case fromObject o of
    Just (a, b) -> liftM2 (:*:) (genericFromObject a) (genericFromObject b)
    _           -> Nothing


