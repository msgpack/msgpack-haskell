{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- | Aeson bridge for MessagePack

module Data.MessagePack.Aeson (
  -- * Conversion functions
  toAeson, fromAeson,

  -- * MessagePack instance for Aeson.Value
  -- $msgpackInstance

  -- * ToJSON and FromJSON instance for MessagePack.Object
  -- $aesonInstances

  -- * Wrapper instances
  AsMessagePack(..),
  AsAeson(..),

  -- * Utility functions
  packAeson, unpackAeson,
  decodeMessagePack, encodeMessagePack,
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.DeepSeq
import           Data.Aeson           as A
import qualified Data.ByteString.Lazy as L
import           Data.Data
import           Data.Either
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe
import           Data.MessagePack     as MP
import           Data.Monoid
import           Data.Scientific
import qualified Data.Text.Encoding   as T
import qualified Data.Vector          as V

-- | Convert MessagePack Object to Aeson Value.
-- If the value unable to convert, it returns Nothing
toAeson :: MP.Object -> Maybe Value
toAeson = \case
  ObjectNil      -> Just Null
  ObjectBool b   -> Just $ Bool b
  ObjectInt n    -> Just $ Number $ fromIntegral n
  ObjectFloat f  -> Just $ Number $ realToFrac f
  ObjectDouble d -> Just $ Number $ realToFrac d
  ObjectStr t    -> Just $ String t
  ObjectBin b    -> String <$> either (const Nothing) Just (T.decodeUtf8' b)
  ObjectArray v  -> Array <$> V.mapM toAeson v
  ObjectMap m    ->
    A.Object . HM.fromList . V.toList
      <$> V.mapM (\(k, v) -> (,) <$> fromObject k <*> toAeson v) m
  ObjectExt _ _  -> Nothing

-- | Convert Aeson Value to MessagePack Object
fromAeson :: Value -> MP.Object
fromAeson = \case
  Null        -> ObjectNil
  Bool b      -> ObjectBool b
  Number s ->
    case floatingOrInteger s of
      Left f  -> ObjectDouble f
      Right n -> ObjectInt n
  String t    -> ObjectStr t
  Array v     -> ObjectArray $ V.map fromAeson v
  A.Object o  -> ObjectMap $ V.fromList $ map (toObject *** fromAeson) $ HM.toList o

-- $msgpackInstance
-- > instance MessagePack Value
instance MessagePack Value where
  fromObject = toAeson
  toObject = fromAeson

-- $aesonInstances
-- > instance ToJSON Object
-- > instance FromJSON Object
instance ToJSON MP.Object where
  -- When fail to convert, it returns `Null`
  toJSON = fromMaybe Null .toAeson

instance FromJSON MP.Object where
  parseJSON = return . fromAeson

-- | Wrapper for using Aeson values as MessagePack value.
newtype AsMessagePack a = AsMessagePack { getAsMessagePack :: a }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable, NFData)

instance (FromJSON a, ToJSON a) => MessagePack (AsMessagePack a) where
  fromObject o = AsMessagePack <$> (fromJSON' =<< toAeson o)
  toObject = fromAeson . toJSON . getAsMessagePack

-- | Wrapper for using MessagePack values as Aeson value.
newtype AsAeson a = AsAeson { getAsAeson :: a }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable, NFData)

instance MessagePack a => ToJSON (AsAeson a) where
  toJSON = fromMaybe Null . toAeson . toObject . getAsAeson

instance MessagePack a => FromJSON (AsAeson a) where
  parseJSON = maybe empty (return . AsAeson) . fromObject . fromAeson

-- | Pack Aeson value to MessagePack binary
packAeson :: ToJSON a => a -> L.ByteString
packAeson = pack . toJSON

-- | Unpack Aeson value from MessagePack binary
unpackAeson :: FromJSON a => L.ByteString -> Maybe a
unpackAeson b = fromJSON' =<< unpack b

-- | Encode MessagePack value to JSON
encodeMessagePack :: MessagePack a => a -> L.ByteString
encodeMessagePack = encode . toJSON . AsAeson

-- | Decode MessagePack value from JSON
decodeMessagePack :: MessagePack a => L.ByteString -> Maybe a
decodeMessagePack b = getAsAeson <$> (fromJSON' =<< decode b)

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = resultToMaybe . fromJSON

resultToMaybe :: Result a -> Maybe a
resultToMaybe = \case
  Success a -> Just a
  _         -> Nothing
