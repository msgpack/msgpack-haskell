{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Data.MessagePack.Generic () where

import           Control.Applicative     (Applicative, (<$>), (<*>))
import           Control.Monad           ((>=>))
import           Data.Bits               (shiftR)
import           Data.Word               (Word64)
import           GHC.Generics

import           Data.MessagePack.Class
import           Data.MessagePack.Object (Object (..))


instance GMessagePack U1 where
  gToObject U1 = ObjectNil
  gFromObject ObjectNil = return U1
  gFromObject _         = fail "invalid encoding for custom unit type"

instance (GMessagePack a, GProdPack b) => GMessagePack (a :*: b) where
  gToObject = toObject . prodToObject
  gFromObject = fromObject >=> prodFromObject

instance (GSumPack a, GSumPack b, SumSize a, SumSize b) => GMessagePack (a :+: b) where
  gToObject = sumToObject 0 size
    where
      size = unTagged (sumSize :: Tagged (a :+: b) Word64)

  gFromObject = \case
    ObjectInt code -> checkSumFromObject0 size (fromIntegral code)
    o              -> fromObject o >>= uncurry (checkSumFromObject size)
    where
      size = unTagged (sumSize :: Tagged (a :+: b) Word64)

instance GMessagePack a => GMessagePack (M1 t c a) where
  gToObject (M1 x) = gToObject x
  gFromObject x = M1 <$> gFromObject x

instance MessagePack a => GMessagePack (K1 i a) where
  gToObject (K1 x) = toObject x
  gFromObject o = K1 <$> fromObject o


-- Product type packing.

class GProdPack f where
  prodToObject :: f a -> [Object]
  prodFromObject :: (Applicative m, Monad m) => [Object] -> m (f a)


instance (GMessagePack a, GProdPack b) => GProdPack (a :*: b) where
  prodToObject (a :*: b) = gToObject a : prodToObject b
  prodFromObject (a:b) = (:*:) <$> gFromObject a <*> prodFromObject b
  prodFromObject _     = fail "invalid encoding for product type"

instance GMessagePack a => GProdPack (M1 t c a) where
  prodToObject (M1 x) = [gToObject x]
  prodFromObject [x] = M1 <$> gFromObject x
  prodFromObject _   = fail "invalid encoding for product type"


-- Sum type packing.

checkSumFromObject0 :: (Applicative m, Monad m) => (GSumPack f) => Word64 -> Word64 -> m (f a)
checkSumFromObject0 size code
  | code < size = sumFromObject code size ObjectNil
  | otherwise   = fail "invalid encoding for sum type"


checkSumFromObject :: (Applicative m, Monad m) => (GSumPack f) => Word64 -> Word64 -> Object -> m (f a)
checkSumFromObject size code x
  | code < size = sumFromObject code size x
  | otherwise   = fail "invalid encoding for sum type"


class GSumPack f where
  sumToObject :: Word64 -> Word64 -> f a -> Object
  sumFromObject :: (Applicative m, Monad m) => Word64 -> Word64 -> Object -> m (f a)


instance (GSumPack a, GSumPack b) => GSumPack (a :+: b) where
  sumToObject code size = \case
    L1 x -> sumToObject code           sizeL x
    R1 x -> sumToObject (code + sizeL) sizeR x
    where
      sizeL = size `shiftR` 1
      sizeR = size - sizeL

  sumFromObject code size x
    | code < sizeL = L1 <$> sumFromObject code           sizeL x
    | otherwise    = R1 <$> sumFromObject (code - sizeL) sizeR x
    where
      sizeL = size `shiftR` 1
      sizeR = size - sizeL


instance GSumPack (C1 c U1) where
  sumToObject code _ _ = toObject code
  sumFromObject _ _ = gFromObject


instance GMessagePack a => GSumPack (C1 c a) where
  sumToObject code _ x = toObject (code, gToObject x)
  sumFromObject _ _ = gFromObject


-- Sum size.

class SumSize f where
  sumSize :: Tagged f Word64

newtype Tagged (s :: * -> *) b = Tagged { unTagged :: b }

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
  sumSize = Tagged $ unTagged (sumSize :: Tagged a Word64) +
                     unTagged (sumSize :: Tagged b Word64)

instance SumSize (C1 c a) where
  sumSize = Tagged 1
