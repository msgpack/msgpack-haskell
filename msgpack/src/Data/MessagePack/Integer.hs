{-# LANGUAGE LambdaCase #-}

-- |
-- Module    : Data.MessagePack.Integer
-- Copyright : © Herbert Valerio Riedel 2019
-- License   : BSD3
--
-- Type representing MessagePack integers
--
module Data.MessagePack.Integer
    ( MPInteger
    , ToMPInteger(..)
    , FromMPInteger(..)

    , putMPInteger
    , getMPInteger
    ) where

import           Control.Applicative
import           Control.DeepSeq     (NFData (rnf))
import           Control.Exception   (ArithException (DivideByZero, Overflow, Underflow),
                                      throw)
import           Data.Int
import           Data.Word

import           Data.Binary         (Binary (get, put))
import           Data.Binary.Get     (Get, getWord16be, getWord32be,
                                      getWord64be, getWord8)
import           Data.Binary.Put     (Put, putWord16be, putWord32be,
                                      putWord64be, putWord8)
import           Data.Bits

-- | Integer type that represents the value range of integral numbers in MessagePack; i.e. \( \left[ -2^{63}, 2^{64}-1 \right] \).
-- In other words, `MPInteger` provides the union of the value ranges of `Word64` and `Int64`.
--
-- This type can be unboxed (i.e. via @{-# UNPACK #-}@).
data MPInteger = MPInteger  {- isW64 -} !Bool
                            {- value -} {-# UNPACK #-} !Int64
  deriving (Eq,Ord)

-- NB: only valid if isW64 is true
toW64 :: Int64 -> Word64
toW64 = fromIntegral


class ToMPInteger a where
  toMPInteger :: a -> MPInteger

instance ToMPInteger Word64 where
  toMPInteger w = MPInteger (i<0) i
    where
      i = fromIntegral w

instance ToMPInteger Int64 where
  toMPInteger = MPInteger False

instance ToMPInteger Int8  where toMPInteger i = MPInteger False (fromIntegral i)
instance ToMPInteger Int16 where toMPInteger i = MPInteger False (fromIntegral i)
instance ToMPInteger Int32 where toMPInteger i = MPInteger False (fromIntegral i)
instance ToMPInteger Int   where toMPInteger i = MPInteger False (fromIntegral i)

instance ToMPInteger Word8  where toMPInteger w = MPInteger False (fromIntegral w)
instance ToMPInteger Word16 where toMPInteger w = MPInteger False (fromIntegral w)
instance ToMPInteger Word32 where toMPInteger w = MPInteger False (fromIntegral w)

instance ToMPInteger Word where
  toMPInteger w = MPInteger (i<0) i
    where
      i = fromIntegral w


-- | Convert a 'MPInteger' value to something else if possible
--
-- The instances for 'FromMPInteger' are supposed to be consistent with the respective instances for 'ToMPInteger', e.g.
--
-- > fromMPInteger . toMPInteger == Just
--
class FromMPInteger a where
  fromMPInteger :: MPInteger -> Maybe a

instance FromMPInteger Word64 where
  fromMPInteger (MPInteger True w) = Just (toW64 w)
  fromMPInteger (MPInteger False i)
    | i < 0     = Nothing
    | otherwise = Just (toW64 i)

instance FromMPInteger Int64 where
  fromMPInteger (MPInteger True _)  = Nothing
  fromMPInteger (MPInteger False i) = Just i



-- NOTE: Internal invariant of 'MPInteger'
--
-- 'isW64' MUST be true IFF the value range of `Int64` cannot represent the semantic value of 'value'
--
-- Consequently, when 'isW64' is true, 'value :: Int64' must be negative.


instance Bounded MPInteger where
  minBound = MPInteger False minBound
  maxBound = MPInteger True  (-1) -- this is why we can't autoderive

instance Enum MPInteger where
  toEnum i = MPInteger False (toEnum i)
  fromEnum (MPInteger True i)  = fromEnum (toW64 i)
  fromEnum (MPInteger False i) = fromEnum i

instance Show MPInteger where
  showsPrec p (MPInteger False v) = showsPrec p v
  showsPrec p (MPInteger True  v) = showsPrec p (toW64 v)

instance NFData MPInteger where
  rnf (MPInteger _ _) = ()

-- | This instance will throw the respective arithmetic 'Underflow' and 'Overflow' exception if the range of 'MPInteger' is exceeded.
instance Num MPInteger where
  fromInteger i
    | i <  toInteger (minBound :: Int64)  = throw Underflow
    | i <= toInteger (maxBound :: Int64)  = MPInteger False (fromInteger i)
    | i <= toInteger (maxBound :: Word64) = MPInteger True (fromInteger i)
    | otherwise = throw Overflow

  negate (MPInteger False v)
    | v == minBound = MPInteger True  v -- NB: for the usual twos complement integers, `negate minBound == minBound`
    | otherwise     = MPInteger False (negate v)
  negate (MPInteger True v)
    | v == minBound = MPInteger False v
    | otherwise     = throw Underflow


  -- addition
  MPInteger False 0 + x = x
  x + MPInteger False 0 = x

  MPInteger True _  + MPInteger True  _ = throw Overflow

  x@(MPInteger True _) + y@(MPInteger False _) = y + x
  MPInteger False y + MPInteger True x
    | y > 0     = if z<0 then MPInteger True z else throw Overflow
    | otherwise = MPInteger (z<0) z
    where
      z = x+y

  MPInteger False y + MPInteger False x
    | x > 0, y > 0, z < 0 = MPInteger True z
    | x < 0, y < 0, z > 0 = throw Underflow
    | otherwise           = MPInteger False z
    where
      z = x+y

  signum (MPInteger True _)  = MPInteger False 1
  signum (MPInteger False v) = MPInteger False (signum v)

  abs v@(MPInteger True _) = v
  abs v0@(MPInteger False v)
    | v >= 0 = v0
    | v == minBound = MPInteger True  v
    | otherwise     = MPInteger False (negate v)


  MPInteger True  _  * MPInteger True  _ = throw Overflow
  MPInteger False 0  * MPInteger _ _     = MPInteger False 0
  MPInteger False 1  * x                 = x
  MPInteger _ _      * MPInteger False 0 = MPInteger False 0
  x                  * MPInteger False 1 = x

  -- cheat
  x * y = fromInteger (toInteger x * toInteger y)

instance Real MPInteger where
  toRational (MPInteger False i) = toRational i
  toRational (MPInteger True u)  = toRational (toW64 u)

instance Integral MPInteger where
  toInteger (MPInteger False i) = toInteger i
  toInteger (MPInteger True u)  = toInteger (toW64 u)

  quotRem _ (MPInteger False 0)    = throw DivideByZero
  quotRem x (MPInteger False 1)    = (x, MPInteger False 0)
  quotRem x (MPInteger False (-1)) = (negate x, MPInteger False 0)

  quotRem (MPInteger False x) (MPInteger False y)
    | (x',y') <- quotRem x y = (MPInteger False x', MPInteger False y')

  -- cheat
  quotRem x y
    | (x',y') <- quotRem (toInteger x) (toInteger y) = (fromInteger x', fromInteger y')

----------------------------------------------------------------------------

-- | This 'Binary' instance encodes\/decodes to\/from MessagePack format
instance Binary MPInteger where
  get = getMPInteger
  put = putMPInteger

-- | Serializes 'MPInteger' to MessagePack
--
-- The shortest encoding is used to serialize
-- 'MPInteger's. Moreoever, for non-negative integers the unsigned
-- encoding is always used.
putMPInteger :: MPInteger -> Put
putMPInteger (MPInteger False i)
    -- positive fixnum stores 7-bit positive integer
    -- negative fixnum stores 5-bit negative integer
  | -32 <= i && i <= 127 = putWord8 (fromIntegral i)

    -- unsigned int encoding
  | i >= 0 = case () of
      _ | i < 0x100       -> putWord8 0xCC >> putWord8     (fromIntegral i)
        | i < 0x10000     -> putWord8 0xCD >> putWord16be  (fromIntegral i)
        | i < 0x100000000 -> putWord8 0xCE >> putWord32be  (fromIntegral i)
        | otherwise       -> putWord8 0xCF >> putWord64be  (fromIntegral i)

    -- signed int encoding
  | -0x80       <= i = putWord8 0xD0 >> putWord8     (fromIntegral i)
  | -0x8000     <= i = putWord8 0xD1 >> putWord16be  (fromIntegral i)
  | -0x80000000 <= i = putWord8 0xD2 >> putWord32be  (fromIntegral i)
  | otherwise        = putWord8 0xD3 >> putWord64be  (fromIntegral i)
putMPInteger (MPInteger True w) = putWord8 0xCF >> putWord64be (toW64 w)

-- | Deserializes 'MPInteger' from MessagePack
--
-- This operation will only fail if a non-integer MessagePack tag is encountered.
getMPInteger :: Get MPInteger
getMPInteger = getWord8 >>= \case
  -- positive fixnum stores 7-bit positive integer
  -- negative fixnum stores 5-bit negative integer
  c | c .&. 0x80 == 0x00 -> pure $! toMPInteger (c :: Word8)
    | c .&. 0xE0 == 0xE0 -> pure $! toMPInteger (fromIntegral c :: Int8)

  0xCC -> toMPInteger <$> getWord8
  0xCD -> toMPInteger <$> getWord16be
  0xCE -> toMPInteger <$> getWord32be
  0xCF -> toMPInteger <$> getWord64be

  0xD0 -> toMPInteger <$> (fromIntegral <$> getWord8    :: Get Int8)
  0xD1 -> toMPInteger <$> (fromIntegral <$> getWord16be :: Get Int16)
  0xD2 -> toMPInteger <$> (fromIntegral <$> getWord32be :: Get Int32)
  0xD3 -> toMPInteger <$> (fromIntegral <$> getWord64be :: Get Int64)

  _    -> empty

