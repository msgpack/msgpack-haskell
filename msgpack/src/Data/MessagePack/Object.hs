{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Object
-- Copyright : (c) Hideyuki Tanaka, 2009-2011
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
  Msgpack(..),
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.DeepSeq
import           Data.Binary
import qualified Data.ByteString          as S
import qualified Data.ByteString.Lazy     as L
import           Data.Hashable
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.IntMap.Strict       as IntMap
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT
import           Data.Typeable
import qualified Data.Vector              as V

import           Data.MessagePack.Assoc
import           Data.MessagePack.Pack
import           Data.MessagePack.Unpack

-- | Object Representation of MessagePack data.
data Object
  = ObjectNil
  | ObjectBool                  !Bool
  | ObjectInt    {-# UNPACK #-} !Int
  | ObjectFloat  {-# UNPACK #-} !Float
  | ObjectDouble {-# UNPACK #-} !Double
  | ObjectRAW                   !S.ByteString
  | ObjectArray                 [Object]
  | ObjectMap                   [(Object, Object)]
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
  <|> ObjectRAW    <$> getRAW
  <|> ObjectArray  <$> getArray getObject
  <|> ObjectMap    <$> getMap getObject getObject

putObject :: Object -> Put
putObject = \case
  ObjectNil      -> putNil
  ObjectBool   b -> putBool b
  ObjectInt    n -> putInt n
  ObjectFloat  f -> putFloat f
  ObjectDouble d -> putDouble d
  ObjectRAW    r -> putRAW r
  ObjectArray  a -> putArray putObject a
  ObjectMap    m -> putMap putObject putObject m

instance Binary Object where
  get = getObject
  put = putObject

class Msgpack a where
  toObject   :: a -> Object
  fromObject :: Object -> Maybe a

-- core instances

instance Msgpack Object where
  toObject = id
  fromObject = Just

instance Msgpack () where
  toObject _ = ObjectNil
  fromObject = \case
    ObjectNil -> Just ()
    _         -> Nothing

instance Msgpack Int where
  toObject = ObjectInt
  fromObject = \case
    ObjectInt n -> Just n
    _           -> Nothing

instance Msgpack Bool where
  toObject = ObjectBool
  fromObject = \case
    ObjectBool b -> Just b
    _            -> Nothing

instance Msgpack Float where
  toObject = ObjectFloat
  fromObject = \case
    ObjectFloat  f -> Just f
    ObjectDouble d -> Just $ realToFrac d
    _              -> Nothing

instance Msgpack Double where
  toObject = ObjectDouble
  fromObject = \case
    ObjectFloat  f -> Just $ realToFrac f
    ObjectDouble d -> Just d
    _              -> Nothing

instance Msgpack S.ByteString where
  toObject = ObjectRAW
  fromObject = \case
    ObjectRAW r -> Just r
    _           -> Nothing

-- Because of overlapping instance, this must be above [a]
instance Msgpack String where
  toObject = toObject . T.encodeUtf8 . T.pack
  fromObject obj = T.unpack . T.decodeUtf8 <$> fromObject obj

instance Msgpack a => Msgpack [a] where
  toObject = ObjectArray . map toObject
  fromObject = \case
    ObjectArray xs -> mapM fromObject xs
    _              -> Nothing

instance (Msgpack a, Msgpack b) => Msgpack (Assoc [(a, b)]) where
  toObject (Assoc xs) = ObjectMap $ map (toObject *** toObject) xs
  fromObject = \case
    ObjectMap xs ->
      Assoc <$> mapM (\(k, v) -> (,) <$> fromObject k <*> fromObject v) xs
    _ ->
      Nothing

-- util instances

-- nullable

instance Msgpack a => Msgpack (Maybe a) where
  toObject = \case
    Just a  -> toObject a
    Nothing -> ObjectNil

  fromObject = \case
    ObjectNil -> Just Nothing
    obj -> fromObject obj

-- UTF8 string like

instance Msgpack L.ByteString where
  toObject = ObjectRAW . L.toStrict
  fromObject obj = L.fromStrict <$> fromObject obj

instance Msgpack T.Text where
  toObject = toObject . T.encodeUtf8
  fromObject obj = T.decodeUtf8With skipChar <$> fromObject obj

instance Msgpack LT.Text where
  toObject = ObjectRAW . L.toStrict . LT.encodeUtf8
  fromObject obj = LT.decodeUtf8With skipChar <$> fromObject obj

skipChar :: T.OnDecodeError
skipChar _ _ = Nothing

-- map like

instance (Msgpack k, Msgpack v) => Msgpack (Assoc (V.Vector (k, v))) where
  toObject = toObject . Assoc . V.toList . unAssoc
  fromObject obj = Assoc . V.fromList . unAssoc <$> fromObject obj

instance (Msgpack k, Msgpack v, Ord k) => Msgpack (Map.Map k v) where
  toObject = toObject . Assoc . Map.toList
  fromObject obj = Map.fromList . unAssoc <$> fromObject obj

instance Msgpack v => Msgpack (IntMap.IntMap v) where
  toObject = toObject . Assoc . IntMap.toList
  fromObject obj = IntMap.fromList . unAssoc <$> fromObject obj

instance (Msgpack k, Msgpack v, Hashable k, Eq k) => Msgpack (HashMap.HashMap k v) where
  toObject = toObject . Assoc . HashMap.toList
  fromObject obj = HashMap.fromList . unAssoc <$> fromObject obj

{-
tryFromObjectError :: Either String a
tryFromObjectError = Left "tryFromObject: cannot cast"

instance (OBJECT a1, OBJECT a2) => OBJECT (a1, a2) where
  toObject (a1, a2) = ObjectArray [toObject a1, toObject a2]
  tryFromObject (ObjectArray arr) =
    case arr of
      [o1, o2] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        return (v1, v2)
      _ ->
        tryFromObjectError
  tryFromObject _ =
    tryFromObjectError

instance (OBJECT a1, OBJECT a2, OBJECT a3) => OBJECT (a1, a2, a3) where
  toObject (a1, a2, a3) = ObjectArray [toObject a1, toObject a2, toObject a3]
  tryFromObject (ObjectArray arr) =
    case arr of
      [o1, o2, o3] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        return (v1, v2, v3)
      _ ->
        tryFromObjectError
  tryFromObject _ =
    tryFromObjectError

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4) => OBJECT (a1, a2, a3, a4) where
  toObject (a1, a2, a3, a4) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4]
  tryFromObject (ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        return (v1, v2, v3, v4)
      _ ->
        tryFromObjectError
  tryFromObject _ =
    tryFromObjectError

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4, OBJECT a5) => OBJECT (a1, a2, a3, a4, a5) where
  toObject (a1, a2, a3, a4, a5) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5]
  tryFromObject (ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4, o5] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        v5 <- tryFromObject o5
        return (v1, v2, v3, v4, v5)
      _ ->
        tryFromObjectError
  tryFromObject _ =
    tryFromObjectError

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4, OBJECT a5, OBJECT a6) => OBJECT (a1, a2, a3, a4, a5, a6) where
  toObject (a1, a2, a3, a4, a5, a6) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6]
  tryFromObject (ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4, o5, o6] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        v5 <- tryFromObject o5
        v6 <- tryFromObject o6
        return (v1, v2, v3, v4, v5, v6)
      _ ->
        tryFromObjectError
  tryFromObject _ =
    tryFromObjectError

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4, OBJECT a5, OBJECT a6, OBJECT a7) => OBJECT (a1, a2, a3, a4, a5, a6, a7) where
  toObject (a1, a2, a3, a4, a5, a6, a7) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7]
  tryFromObject (ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4, o5, o6, o7] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        v5 <- tryFromObject o5
        v6 <- tryFromObject o6
        v7 <- tryFromObject o7
        return (v1, v2, v3, v4, v5, v6, v7)
      _ ->
        tryFromObjectError
  tryFromObject _ =
    tryFromObjectError

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4, OBJECT a5, OBJECT a6, OBJECT a7, OBJECT a8) => OBJECT (a1, a2, a3, a4, a5, a6, a7, a8) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8]
  tryFromObject (ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4, o5, o6, o7, o8] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        v5 <- tryFromObject o5
        v6 <- tryFromObject o6
        v7 <- tryFromObject o7
        v8 <- tryFromObject o8
        return (v1, v2, v3, v4, v5, v6, v7, v8)
      _ ->
        tryFromObjectError
  tryFromObject _ =
    tryFromObjectError

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4, OBJECT a5, OBJECT a6, OBJECT a7, OBJECT a8, OBJECT a9) => OBJECT (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8, a9) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8, toObject a9]
  tryFromObject (ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4, o5, o6, o7, o8, o9] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        v5 <- tryFromObject o5
        v6 <- tryFromObject o6
        v7 <- tryFromObject o7
        v8 <- tryFromObject o8
        v9 <- tryFromObject o9
        return (v1, v2, v3, v4, v5, v6, v7, v8, v9)
      _ ->
        tryFromObjectError
  tryFromObject _ =
    tryFromObjectError

-}
