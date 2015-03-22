{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

-- | Aeson adapter for MessagePack

module Data.MessagePack.Aeson (
  ) where

import           Control.Applicative
import           Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Either
import qualified Data.HashMap.Strict  as HM
import           Data.MessagePack     as MP
import           Data.Scientific
import qualified Data.Text.Encoding   as T
import qualified Data.Vector          as V

fromMP :: MP.Object -> Maybe Value
fromMP = \case
  ObjectNil      -> Just $ Null
  ObjectBool b   -> Just $ Bool b
  ObjectInt n    -> Just $ Number $ fromIntegral n
  ObjectFloat f  -> Just $ Number $ realToFrac f
  ObjectDouble d -> Just $ Number $ realToFrac d
  ObjectRAW b    -> String <$> either (const Nothing) Just (T.decodeUtf8' b)
  ObjectArray v  -> Array <$> V.mapM fromMP v
  ObjectMap m ->
    Aeson.Object . HM.fromList . V.toList
      <$> V.mapM (\(k, v) -> (,) <$> fromObject k <*> fromMP v) m

toMP :: Value -> MP.Object
toMP = \case
  Aeson.Object o -> ObjectMap $ V.fromList $ map (\(k, v) -> (toObject k, toMP v)) $ HM.toList o
  Array v        -> ObjectArray $ V.map toMP v
  String t       -> ObjectRAW $ T.encodeUtf8 t
  Number s ->
    case floatingOrInteger s of
      Left f     -> ObjectDouble f
      Right n    -> ObjectInt n
  Bool b         -> ObjectBool b
  Null           -> ObjectNil

newtype AsAeson = AsAeson { getAsAeson :: MP.Object }

unpackMP :: FromJSON a => L.ByteString -> a
unpackMP = undefined

packMP :: ToJSON a => a -> L.ByteString
packMP = undefined
