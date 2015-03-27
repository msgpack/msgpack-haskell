{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, DeriveFunctor, DeriveDataTypeable #-}

-- | Aeson bridge for MessagePack

module Data.MessagePack.Aeson (
  -- * Conversion functions
  toAeson, fromAeson,

  -- * MessagePack instance for Aeson.Value
  -- * ToJSON, FromJSON instance for MessagePack.Object

  -- * Wrapper instances
  AsMessagePack(..),
  AsAeson(..),

  -- * Utility functions
  packAeson, unpackAeson,
  decodeMessagePack, encodeMessagePack,
  ) where

import           Control.Applicative
import           Data.Aeson           as A
import qualified Data.ByteString.Lazy as L
import           Data.Either
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe
import           Data.MessagePack     as MP
import           Data.Monoid
import           Data.Scientific
import qualified Data.Text.Encoding   as T
import qualified Data.Vector          as V
import Data.Data
import Control.DeepSeq
import Control.Arrow

toAeson :: MP.Object -> Maybe Value
toAeson = \case
  ObjectNil      -> Just Null
  ObjectBool b   -> Just $ Bool b
  ObjectInt n    -> Just $ Number $ fromIntegral n
  ObjectFloat f  -> Just $ Number $ realToFrac f
  ObjectDouble d -> Just $ Number $ realToFrac d
  ObjectRAW b    -> String <$> either (const Nothing) Just (T.decodeUtf8' b)
  ObjectArray v  -> Array <$> V.mapM toAeson v
  ObjectMap m ->
    A.Object . HM.fromList . V.toList
      <$> V.mapM (\(k, v) -> (,) <$> fromObject k <*> toAeson v) m

fromAeson :: Value -> MP.Object
fromAeson = \case
  Null        -> ObjectNil
  Bool b      -> ObjectBool b
  Number s ->
    case floatingOrInteger s of
      Left f  -> ObjectDouble f
      Right n -> ObjectInt n
  String t    -> ObjectRAW $ T.encodeUtf8 t
  Array v     -> ObjectArray $ V.map fromAeson v
  A.Object o  -> ObjectMap $ V.fromList $ map (toObject *** fromAeson) $ HM.toList o

instance MessagePack Value where
  fromObject = toAeson
  toObject = fromAeson

instance ToJSON MP.Object where
  -- | When fail to convert, it returns `Null`
  toJSON = fromMaybe Null .toAeson

instance FromJSON MP.Object where
  parseJSON = return . fromAeson

newtype AsMessagePack a = AsMessagePack { getAsMessagePack :: a }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable, NFData)

instance (FromJSON a, ToJSON a) => MessagePack (AsMessagePack a) where
  fromObject o = AsMessagePack <$> (fromJSON' =<< toAeson o)
  toObject = fromAeson . toJSON . getAsMessagePack

newtype AsAeson a = AsAeson { getAsAeson :: a }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable, NFData)

instance MessagePack a => ToJSON (AsAeson a) where
  toJSON = fromMaybe Null . toAeson . toObject . getAsAeson

instance MessagePack a => FromJSON (AsAeson a) where
  parseJSON = maybe empty (return . AsAeson) . fromObject . fromAeson

-- | pack Aeson value to msgpack binary
packAeson :: ToJSON a => a -> L.ByteString
packAeson = pack . toJSON

-- | unpack Aeson value from msgpack binary
unpackAeson :: FromJSON a => L.ByteString -> Maybe a
unpackAeson b = fromJSON' =<< unpack b

encodeMessagePack :: MessagePack a => a -> L.ByteString
encodeMessagePack = encode . toJSON . AsAeson

decodeMessagePack :: MessagePack a => L.ByteString -> Maybe a
decodeMessagePack b = getAsAeson <$> (fromJSON' =<< decode b)

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = resultToMaybe . fromJSON

resultToMaybe :: Result a -> Maybe a
resultToMaybe = \case
  Success a -> Just a
  _         -> Nothing
