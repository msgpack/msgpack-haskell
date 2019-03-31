{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Object
-- Copyright : © Hideyuki Tanaka 2009-2015
--           , © Herbert Valerio Riedel 2019
-- License   : BSD3
--
-- MessagePack object definition
--
--------------------------------------------------------------------

module Data.MessagePack.Object (
  -- * MessagePack Object
  Object(..),

  -- * MessagePack Serializable Types
  MessagePack(..),
  ) where

import           Compat.Prelude
import           Prelude                  hiding (putStr)

import           Control.Arrow
import qualified Data.ByteString          as S
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteString.Short    as SBS
import           Data.Hashable            (Hashable)
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.IntMap.Strict       as IntMap
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Vector              as V

import           Data.MessagePack.Assoc
import           Data.MessagePack.Get
import           Data.MessagePack.Integer
import           Data.MessagePack.Put

import           Compat.Binary


-- | Object Representation of MessagePack data.
--
-- @since 1.1.0.0
data Object
  = ObjectNil
    -- ^ represents nil
  | ObjectBool                  !Bool
    -- ^ represents true or false
  | ObjectInt    {-# UNPACK #-} !MPInteger
    -- ^ represents an integer (__NOTE__: Changed from 'Int' to 'MPInteger' in @msgpack-1.1.0.0@)
  | ObjectFloat  {-# UNPACK #-} !Float
    -- ^ represents a floating point number
  | ObjectDouble {-# UNPACK #-} !Double
    -- ^ represents a floating point number
  | ObjectStr                   !T.Text
    -- ^ represents an UTF-8 string
    --
    -- __NOTE__: MessagePack is limited to maximum UTF-8 encoded size of \( 2^{32}-1 \) octets.
  | ObjectBin                   !S.ByteString
    -- ^ represents opaque binary data
    --
    -- __NOTE__: MessagePack is limited to maximum data size of \( 2^{32}-1 \) bytes.
  | ObjectArray                 !(V.Vector Object)
    -- ^ represents a sequence of objects
    --
    -- __NOTE__: MessagePack is limited to maximum of \( 2^{32}-1 \) array items.
  | ObjectMap                   !(V.Vector (Object, Object))
    -- ^ represents key-value pairs of objects
    --
    -- __NOTE__: MessagePack is limited to maximum of \( 2^{32}-1 \) map entries.
  | ObjectExt    {-# UNPACK #-} !Int8 !S.ByteString
    -- ^ represents a tuple of an integer and a byte array where
    -- the signed 8-bit represents type information and the byte array represents data.
    -- Negative type-ids are reserved for use by the MessagePack specification; in other words, only the use of the type values @[ 0 .. 127 ]@ is allowed for custom extension data.
    --
    -- See "Data.MessagePack.Timestamp" for dealing with the MessagePack defined extension type @-1@.
    --
    -- __NOTE__: MessagePack is limited to maximum extension data size of up to \( 2^{32}-1 \) bytes.
  deriving (Show, Read, Eq, Ord, Typeable, Generic)

instance NFData Object where
  rnf obj = case obj of
    ObjectArray a -> rnf a
    ObjectMap   m -> rnf m
    _             -> ()

getObject :: Get Object
getObject =
      ObjectNil    <$  getNil
  <|> ObjectBool   <$> getBool
  <|> ObjectInt    <$> get
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
  ObjectInt    n -> put n
  ObjectFloat  f -> putFloat f
  ObjectDouble d -> putDouble d
  ObjectStr    t -> putStr t
  ObjectBin    b -> putBin b
  ObjectArray  a -> putArray putObject a
  ObjectMap    m -> putMap putObject putObject m
  ObjectExt  b r -> putExt b r

-- | This 'Binary' instance encodes\/decodes to\/from MessagePack format
instance Binary Object where
  get = getObject
  put = putObject

-- | Class for converting between MessagePack 'Object's and native Haskell types.
class MessagePack a where
  toObject   :: a -> Object
  fromObject :: Object -> Maybe a

-- core instances

-- | The trivial identity 'MessagePack' instance
instance MessagePack Object where
  toObject = id
  fromObject = Just

-- | Encodes as 'ObjectNil'
instance MessagePack () where
  toObject _ = ObjectNil
  fromObject = \case
    ObjectNil -> Just ()
    _         -> Nothing

instance MessagePack Bool where
  toObject = ObjectBool
  fromObject = \case
    ObjectBool b -> Just b
    _            -> Nothing

----------------------------------------------------------------------------

-- | @since 1.1.0.0
instance MessagePack MPInteger where
  toObject = ObjectInt
  fromObject = \case
    ObjectInt n -> Just n
    _           -> Nothing

-- | @since 1.1.0.0
instance MessagePack Word64 where
  toObject = ObjectInt . toMPInteger
  fromObject = \case
    ObjectInt n -> fromMPInteger n
    _           -> Nothing

-- | @since 1.1.0.0
instance MessagePack Word32 where
  toObject = ObjectInt . toMPInteger
  fromObject = \case
    ObjectInt n -> fromMPInteger n
    _           -> Nothing

-- | @since 1.1.0.0
instance MessagePack Word16 where
  toObject = ObjectInt . toMPInteger
  fromObject = \case
    ObjectInt n -> fromMPInteger n
    _           -> Nothing

-- | @since 1.1.0.0
instance MessagePack Word8 where
  toObject = ObjectInt . toMPInteger
  fromObject = \case
    ObjectInt n -> fromMPInteger n
    _           -> Nothing

-- | @since 1.1.0.0
instance MessagePack Word where
  toObject = ObjectInt . toMPInteger
  fromObject = \case
    ObjectInt n -> fromMPInteger n
    _           -> Nothing


-- | @since 1.1.0.0
instance MessagePack Int64 where
  toObject = ObjectInt . toMPInteger
  fromObject = \case
    ObjectInt n -> fromMPInteger n
    _           -> Nothing

-- | @since 1.1.0.0
instance MessagePack Int32 where
  toObject = ObjectInt . toMPInteger
  fromObject = \case
    ObjectInt n -> fromMPInteger n
    _           -> Nothing

-- | @since 1.1.0.0
instance MessagePack Int16 where
  toObject = ObjectInt . toMPInteger
  fromObject = \case
    ObjectInt n -> fromMPInteger n
    _           -> Nothing

-- | @since 1.1.0.0
instance MessagePack Int8 where
  toObject = ObjectInt . toMPInteger
  fromObject = \case
    ObjectInt n -> fromMPInteger n
    _           -> Nothing

instance MessagePack Int where
  toObject = ObjectInt . toMPInteger
  fromObject = \case
    ObjectInt n -> fromMPInteger n
    _           -> Nothing

----------------------------------------------------------------------------

instance MessagePack Float where
  toObject = ObjectFloat
  fromObject = \case
    ObjectInt    n -> Just $! fromIntegral n
    ObjectFloat  f -> Just f
    ObjectDouble d -> Just $! realToFrac d
    _              -> Nothing

instance MessagePack Double where
  toObject = ObjectDouble
  fromObject = \case
    ObjectInt    n -> Just $! fromIntegral n
    ObjectFloat  f -> Just $! realToFrac f
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

-- nullable

-- | 'Maybe's are encoded as nullable types, i.e. 'Nothing' is encoded as @nil@.
--
-- __NOTE__: Encoding nested 'Maybe's or 'Maybe's enclosing types which encode to @nil@ (such as '()') will break round-tripping
instance MessagePack a => MessagePack (Maybe a) where
  toObject = \case
    Just a  -> toObject a
    Nothing -> ObjectNil

  fromObject = \case
    ObjectNil -> Just Nothing
    obj -> Just <$> fromObject obj

-- UTF8 string like

instance MessagePack L.ByteString where
  toObject = ObjectBin . L.toStrict
  fromObject obj = L.fromStrict <$> fromObject obj

-- | @since 1.0.1.0
instance MessagePack SBS.ShortByteString where
  toObject = ObjectBin . SBS.fromShort
  fromObject obj = SBS.toShort <$> fromObject obj

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

-- tuples

instance (MessagePack a1, MessagePack a2) => MessagePack (a1, a2) where
  toObject (a1, a2) = ObjectArray [toObject a1, toObject a2]
  fromObject (ObjectArray [a1, a2]) = (,) <$> fromObject a1 <*> fromObject a2
  fromObject _                      = Nothing

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
