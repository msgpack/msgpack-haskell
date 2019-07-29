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

  -- * MessagePack conveniences
  (.:), (.=),

  -- * MessagePack Serializable Types
  MessagePack(..), typeMismatch, Result(..)
  ) where

import           Compat.Prelude
import           Prelude                       hiding (putStr)

import           Control.Arrow
import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Short         as SBS
import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.IntMap.Strict            as IntMap
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty            as NEL
import qualified Data.Map                      as Map
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           Data.Typeable
import qualified Data.Vector                   as V

import           Data.MessagePack.Assoc
import           Data.MessagePack.Get.Internal
import           Data.MessagePack.Integer
import           Data.MessagePack.Put
import           Data.MessagePack.Result
import           Data.MessagePack.Tags

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

(.:) :: MessagePack a => Object -> T.Text -> Result a
(ObjectMap m) .: key =
  let finder ((ObjectStr k), _) | k == key = True
      finder _ = False
  in case V.find finder m of
    Just (_, a)  -> fromObject a
    _ -> Error $ "missing key " <> T.unpack key
m .: _ = Error $ "expected Objectmap got " <> (show . typeOf $ m)

(.=) :: MessagePack a => T.Text -> a -> (Object, Object)
k .= a = (ObjectStr k, toObject a)

instance NFData Object where
  rnf obj = case obj of
    ObjectArray a -> rnf a
    ObjectMap   m -> rnf m
    _             -> ()

getObject :: Get Object
getObject = do
  -- NB: <|> has the side-effect of un-consuming on failure
  tag <- do { t <- getWord8; guard (t /= TAG_reserved_C1); pure t }
         <|> (fail "encountered reserved MessagePack tag 0xC1")

  tryNil tag (const ObjectNil) $
    tryBool                    tag ObjectBool $
    tryMPInteger               tag ObjectInt $
    tryFloat                   tag ObjectFloat $
    tryDouble                  tag ObjectDouble $
    tryStr                     tag ObjectStr $
    tryBin                     tag ObjectBin $
    tryArray getObject         tag ObjectArray $
    tryMap getObject getObject tag ObjectMap $
    tryExt                     tag (uncurry ObjectExt) $
    fail ("getObject: internal error " ++ show tag) -- should never happen

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

  -- | Encodes directly to 'Put' monad bypassing the intermediate 'Object' AST
  --
  -- @since 1.1.0.0
  toBinary :: a -> Put
  toBinary = putObject . toObject

  fromObject :: Object -> Result a

-- core instances

-- | The trivial identity 'MessagePack' instance
instance MessagePack Object where
  toObject = id
  toBinary = putObject
  fromObject = pure

-- | Encodes as 'ObjectNil'
instance MessagePack () where
  toObject _ = ObjectNil
  toBinary _ = putNil
  fromObject = withNil "()" (pure ())

instance MessagePack Bool where
  toObject = ObjectBool
  toBinary = putBool
  fromObject = withBool "Bool" pure

----------------------------------------------------------------------------

-- | @since 1.1.0.0
instance MessagePack MPInteger where
  toObject = ObjectInt
  toBinary = put
  fromObject = withInt "MPInteger" pure

fromObjectInt :: FromMPInteger i => String -> Object -> Result i
fromObjectInt expected = withInt expected go
  where
    go j = case fromMPInteger j of
      Just j' -> pure j'
      Nothing -> fail ("MessagePack integer " ++ show j ++ " cannot be decoded into " ++ expected)

-- | @since 1.1.0.0
instance MessagePack Word64 where
  toObject = ObjectInt . toMPInteger
  toBinary = put . toMPInteger
  fromObject = fromObjectInt "Word64"

-- | @since 1.1.0.0
instance MessagePack Word32 where
  toObject = ObjectInt . toMPInteger
  toBinary = put . toMPInteger
  fromObject = fromObjectInt "Word32"

-- | @since 1.1.0.0
instance MessagePack Word16 where
  toObject = ObjectInt . toMPInteger
  toBinary = put . toMPInteger
  fromObject = fromObjectInt "Word16"

-- | @since 1.1.0.0
instance MessagePack Word8 where
  toObject = ObjectInt . toMPInteger
  toBinary = put . toMPInteger
  fromObject = fromObjectInt "Word8"

-- | @since 1.1.0.0
instance MessagePack Word where
  toObject = ObjectInt . toMPInteger
  toBinary = put . toMPInteger
  fromObject = fromObjectInt "Word"

-- | @since 1.1.0.0
instance MessagePack Int64 where
  toObject = ObjectInt . toMPInteger
  toBinary = put . toMPInteger
  fromObject = fromObjectInt "Int64"

-- | @since 1.1.0.0
instance MessagePack Int32 where
  toObject = ObjectInt . toMPInteger
  toBinary = put . toMPInteger
  fromObject = fromObjectInt "Int32"

-- | @since 1.1.0.0
instance MessagePack Int16 where
  toObject = ObjectInt . toMPInteger
  toBinary = put . toMPInteger
  fromObject = fromObjectInt "Int16"

-- | @since 1.1.0.0
instance MessagePack Int8 where
  toObject = ObjectInt . toMPInteger
  toBinary = put . toMPInteger
  fromObject = fromObjectInt "Int8"

instance MessagePack Int where
  toObject = ObjectInt . toMPInteger
  toBinary = put . toMPInteger
  fromObject = fromObjectInt "Int"

----------------------------------------------------------------------------

-- | This instance decodes only 32bit floats and will fail to decode 64bit floats from MessagePack streams
instance MessagePack Float where
  toObject = ObjectFloat
  toBinary = putFloat
  fromObject = withFloat "Float" pure

-- | This instance decodes 64bit and 32bit floats from MessagePack streams into a 'Double'
instance MessagePack Double where
  toObject = ObjectDouble
  toBinary = putDouble
  fromObject = withDouble "Double" pure

instance MessagePack S.ByteString where
  toObject = ObjectBin
  toBinary = putBin
  fromObject = withBin "ByteString" pure

-- Because of overlapping instance, this must be above [a]
instance MessagePack String where
  toObject = toObject . T.pack
  toBinary = putStr . T.pack
  fromObject obj = T.unpack <$> fromObject obj

instance MessagePack a => MessagePack (V.Vector a) where
  toObject = ObjectArray . V.map toObject
  toBinary = putArray toBinary
  fromObject = withArray "Vector" (V.mapM fromObject)

instance (MessagePack a, MessagePack b) => MessagePack (Assoc (V.Vector (a, b))) where
  toObject (Assoc xs) = ObjectMap $ V.map (toObject *** toObject) xs
  toBinary (Assoc xs) = putMap toBinary toBinary xs
  fromObject = withMap "Assoc" (fmap Assoc . (V.mapM (\(k, v) -> (,) <$> fromObject k <*> fromObject v)))

-- | 'Maybe's are encoded as nullable types, i.e. 'Nothing' is encoded as @nil@.
--
-- __NOTE__: Encoding nested 'Maybe's or 'Maybe's enclosing types which encode to @nil@ (such as '()') will break round-tripping
instance MessagePack a => MessagePack (Maybe a) where
  toObject = \case
    Just a  -> toObject a
    Nothing -> ObjectNil
  toBinary = \case
    Just a  -> toBinary a
    Nothing -> putNil

  fromObject = \case
    ObjectNil -> pure Nothing
    obj       -> Just <$> fromObject obj

-- UTF8 string like

instance MessagePack L.ByteString where
  toObject = ObjectBin . L.toStrict
  toBinary = putBin . L.toStrict
  fromObject obj = L.fromStrict <$> fromObject obj

-- | @since 1.0.1.0
instance MessagePack SBS.ShortByteString where
  toObject = ObjectBin . SBS.fromShort
  toBinary = putBin . SBS.fromShort
  fromObject obj = SBS.toShort <$> fromObject obj

instance MessagePack T.Text where
  toObject = ObjectStr
  toBinary = putStr
  fromObject = withStr "Text" pure

instance MessagePack LT.Text where
  toObject = toObject . LT.toStrict
  toBinary = putStr . LT.toStrict
  fromObject obj = LT.fromStrict <$> fromObject obj

-- array like

instance MessagePack a => MessagePack [a] where
  toObject = toObject . V.fromList
  toBinary = putArray toBinary . V.fromList
  fromObject obj = V.toList <$> fromObject obj

instance MessagePack a => MessagePack (NonEmpty a) where
  toObject = toObject . NEL.toList
  toBinary = toBinary . NEL.toList
  fromObject o = do
    lst <- fromObject o
    case NEL.nonEmpty lst of
      Just as -> Success as
      Nothing -> Error "empty list"

-- map like

instance (MessagePack k, MessagePack v) => MessagePack (Assoc [(k, v)]) where
  toObject = toObject . Assoc . V.fromList . unAssoc
  toBinary = putMap toBinary toBinary . V.fromList . unAssoc
  fromObject obj = Assoc . V.toList . unAssoc <$> fromObject obj

instance (MessagePack k, MessagePack v, Ord k) => MessagePack (Map.Map k v) where
  toObject = toObject . Assoc . Map.toList
  toBinary = putMap toBinary toBinary . V.fromList . Map.toList
  fromObject obj = Map.fromList . unAssoc <$> fromObject obj

instance MessagePack v => MessagePack (IntMap.IntMap v) where
  toObject = toObject . Assoc . IntMap.toList
  toBinary = putMap toBinary toBinary . V.fromList . IntMap.toList
  fromObject obj = IntMap.fromList . unAssoc <$> fromObject obj

instance (MessagePack k, MessagePack v, Hashable k, Eq k) => MessagePack (HashMap.HashMap k v) where
  toObject = toObject . Assoc . HashMap.toList
  toBinary = putMap toBinary toBinary . V.fromList . HashMap.toList
  fromObject obj = HashMap.fromList . unAssoc <$> fromObject obj

-- tuples

instance (MessagePack a1, MessagePack a2) => MessagePack (a1, a2) where
  toObject (a1, a2) = ObjectArray [toObject a1, toObject a2]
  toBinary (a1, a2) = putArray' 2 $ do { toBinary a1; toBinary a2 }
  fromObject (ObjectArray [a1, a2]) = (,) <$> fromObject a1 <*> fromObject a2
  fromObject obj                    = typeMismatch "2-tuple" obj

instance (MessagePack a1, MessagePack a2, MessagePack a3) => MessagePack (a1, a2, a3) where
  toObject (a1, a2, a3) = ObjectArray [toObject a1, toObject a2, toObject a3]
  toBinary (a1, a2, a3) = putArray' 3 $ do { toBinary a1; toBinary a2; toBinary a3 }
  fromObject (ObjectArray [a1, a2, a3]) = (,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3
  fromObject obj = typeMismatch "3-tuple" obj

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4) => MessagePack (a1, a2, a3, a4) where
  toObject (a1, a2, a3, a4) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4]
  toBinary (a1, a2, a3, a4) = putArray' 4 $ do { toBinary a1; toBinary a2; toBinary a3; toBinary a4 }
  fromObject (ObjectArray [a1, a2, a3, a4]) = (,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4
  fromObject obj = typeMismatch "4-tuple" obj

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5) => MessagePack (a1, a2, a3, a4, a5) where
  toObject (a1, a2, a3, a4, a5) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5]
  toBinary (a1, a2, a3, a4, a5) = putArray' 5 $ do { toBinary a1; toBinary a2; toBinary a3; toBinary a4; toBinary a5 }
  fromObject (ObjectArray [a1, a2, a3, a4, a5]) = (,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5
  fromObject obj = typeMismatch "5-tuple" obj

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6) => MessagePack (a1, a2, a3, a4, a5, a6) where
  toObject (a1, a2, a3, a4, a5, a6) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6]
  toBinary (a1, a2, a3, a4, a5, a6) = putArray' 6 $ do { toBinary a1; toBinary a2; toBinary a3; toBinary a4; toBinary a5; toBinary a6 }
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6]) = (,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6
  fromObject obj = typeMismatch "6-tuple" obj

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7) => MessagePack (a1, a2, a3, a4, a5, a6, a7) where
  toObject (a1, a2, a3, a4, a5, a6, a7) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7]
  toBinary (a1, a2, a3, a4, a5, a6, a7) = putArray' 7 $ do { toBinary a1; toBinary a2; toBinary a3; toBinary a4; toBinary a5; toBinary a6; toBinary a7 }
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7]) = (,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7
  fromObject obj = typeMismatch "7-tuple" obj

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7, MessagePack a8) => MessagePack (a1, a2, a3, a4, a5, a6, a7, a8) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8]
  toBinary (a1, a2, a3, a4, a5, a6, a7, a8) = putArray' 8 $ do { toBinary a1; toBinary a2; toBinary a3; toBinary a4; toBinary a5; toBinary a6; toBinary a7; toBinary a8 }
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7, a8]) = (,,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7 <*> fromObject a8
  fromObject obj = typeMismatch "8-tuple" obj

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7, MessagePack a8, MessagePack a9) => MessagePack (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8, a9) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8, toObject a9]
  toBinary (a1, a2, a3, a4, a5, a6, a7, a8, a9) = putArray' 8 $ do { toBinary a1; toBinary a2; toBinary a3; toBinary a4; toBinary a5; toBinary a6; toBinary a7; toBinary a8; toBinary a9 }
  fromObject (ObjectArray [a1, a2, a3, a4, a5, a6, a7, a8, a9]) = (,,,,,,,,) <$> fromObject a1 <*> fromObject a2 <*> fromObject a3 <*> fromObject a4 <*> fromObject a5 <*> fromObject a6 <*> fromObject a7 <*> fromObject a8 <*> fromObject a9
  fromObject obj = typeMismatch "9-tuple" obj

typeMismatch :: String -> Object -> Result a
typeMismatch expected obj = fail ("MessagePack " ++ got ++ " type cannot be decoded into " ++ expected)
  where
    got = case obj of
      ObjectNil      -> "nil"
      ObjectArray v  -> "array["++show (V.length v)++"]"
      ObjectMap v    -> "map["++show (V.length v)++"]"
      ObjectStr _    -> "str"
      ObjectBool _   -> "bool"
      ObjectInt _    -> "int"
      ObjectFloat _  -> "float32"
      ObjectDouble _ -> "float64"
      ObjectBin _    -> "bin"
      ObjectExt ty _ -> "ext["++show ty++"]"

withNil :: String -> Result a -> Object -> Result a
withNil _ f ObjectNil  = f
withNil expected _ got = typeMismatch expected got

withBool :: String -> (Bool -> Result a) -> Object -> Result a
withBool _ f (ObjectBool b) = f b
withBool expected _ got     = typeMismatch expected got

withInt :: String -> (MPInteger -> Result a) -> Object -> Result a
withInt _ f (ObjectInt i) = f i
withInt expected _ got    = typeMismatch expected got

withFloat :: String -> (Float -> Result a) -> Object -> Result a
withFloat _ f (ObjectFloat x) = f x
withFloat expected _ got      = typeMismatch expected got

withDouble :: String -> (Double -> Result a) -> Object -> Result a
withDouble _ f (ObjectFloat x)  = f $! (realToFrac x)
withDouble _ f (ObjectDouble x) = f x
withDouble expected _ got       = typeMismatch expected got

withBin :: String -> (S.ByteString -> Result a) -> Object -> Result a
withBin _ f (ObjectBin i) = f i
withBin expected _ got    = typeMismatch expected got

withStr :: String -> (T.Text -> Result a) -> Object -> Result a
withStr _ f (ObjectStr i) = f i
withStr expected _ got    = typeMismatch expected got

withArray :: String -> (V.Vector Object -> Result a) -> Object -> Result a
withArray _ f (ObjectArray xs) = f xs
withArray expected _ got       = typeMismatch expected got

withMap :: String -> (V.Vector (Object,Object) -> Result a) -> Object -> Result a
withMap _ f (ObjectMap xs) = f xs
withMap expected _ got     = typeMismatch expected got
