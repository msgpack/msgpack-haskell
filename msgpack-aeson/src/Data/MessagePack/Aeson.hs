{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- | Aeson bridge for MessagePack
module Data.MessagePack.Aeson (
  -- * Conversion functions
  toAeson, fromAeson,

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
import qualified Data.ByteString.Lazy as L (ByteString)
import           Data.Data
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe
import           Data.MessagePack     as MP
import           Data.Scientific
import qualified Data.Text.Encoding   as T
import qualified Data.Vector          as V

-- | Convert 'MP.Object' to JSON 'Value'
toAeson :: MP.Object -> A.Result Value
toAeson = \case
  ObjectNil      -> pure Null
  ObjectBool b   -> pure . Bool $ b
  ObjectInt n    -> pure . Number $ fromIntegral n
  ObjectFloat f  -> pure . Number $ realToFrac f
  ObjectDouble d -> pure . Number $ realToFrac d
  ObjectStr t    -> pure . String $ t
  ObjectBin b    -> String <$> either (fail . show) pure (T.decodeUtf8' b)
  ObjectArray v  -> Array <$> V.mapM toAeson v
  ObjectMap m    ->
    A.Object . HM.fromList . V.toList
      <$> V.mapM (\(k, v) -> (,) <$> from k <*> toAeson v) m
      where from = maybe (fail "bad object") pure . MP.fromObject
  ObjectExt _ _  -> fail "ObjectExt is not supported"

-- | Convert JSON 'Value' to 'MP.Object'
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

-- | Wrapper for using Aeson values as MessagePack value.
newtype AsMessagePack a = AsMessagePack { getAsMessagePack :: a }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable, NFData)

instance (FromJSON a, ToJSON a) => MessagePack (AsMessagePack a) where
  fromObject o = AsMessagePack <$> (\case { A.Error _ -> Nothing; A.Success x -> Just x } $ (fromJSON =<< toAeson o))
  toObject = fromAeson . toJSON . getAsMessagePack

-- | Wrapper for using MessagePack values as Aeson value.
newtype AsAeson a = AsAeson { getAsAeson :: a }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable, NFData)

instance MessagePack a => ToJSON (AsAeson a) where
  toJSON = \case { A.Error _ -> Null; A.Success x -> x } . toAeson . toObject . getAsAeson

instance MessagePack a => FromJSON (AsAeson a) where
  parseJSON = maybe empty (return . AsAeson) . fromObject . fromAeson

-- | Encode to MessagePack via "Data.Aeson"'s 'ToJSON' instances
packAeson :: ToJSON a => a -> L.ByteString
packAeson = pack . fromAeson . toJSON

-- | Decode from MessagePack via "Data.Aeson"'s 'FromJSON' instances
unpackAeson :: FromJSON a => L.ByteString -> Result a
unpackAeson b = fromJSON =<< toAeson =<< maybe (fail "unpackAeson") pure (unpack b)

-- | Encode MessagePack value to JSON document
encodeMessagePack :: MessagePack a => a -> L.ByteString
encodeMessagePack = encode . toJSON . AsAeson

-- | Decode MessagePack value from JSON document
decodeMessagePack :: MessagePack a => L.ByteString -> A.Result a
decodeMessagePack b = getAsAeson <$> (fromJSON =<< either A.Error A.Success (eitherDecode b))
