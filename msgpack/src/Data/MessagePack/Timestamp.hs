{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module    : Data.MessagePack.Integer
-- Copyright : © Herbert Valerio Riedel 2019
-- License   : BSD3
--
-- The 'MPTimestamp' type for representing MessagePack Timestamps
--
-- @since 1.1.0.0
module Data.MessagePack.Timestamp
    ( MPTimestamp

    , mptsFromPosixSeconds
    , mptsFromPosixSeconds2
    , mptsToPosixSeconds2

    , mptsFromPosixNanoseconds
    , mptsToPosixNanoseconds

    , mptsToUTCTime
    , mptsFromUTCTime
    , mptsFromUTCTimeLossy
    ) where

import           Compat.Prelude

import qualified Data.ByteString         as S
import           Data.Fixed
import qualified Data.Time.Clock         as Time
import qualified Data.Time.Clock.POSIX   as Time

import           Compat.Binary           as Bin
import           Data.MessagePack.Get
import           Data.MessagePack.Object
import           Data.MessagePack.Put
import           Data.MessagePack.Tags

-- | A MessagePack timestamp
--
-- The representable range is @[-292277022657-01-27 08:29:52 UTC .. 292277026596-12-04 15:30:07.999999999 UTC]@ with nanosecond precision.
--
-- @since 1.1.0.0
data MPTimestamp = MPTimestamp !Int64 !Word32
                 deriving (Eq,Ord,Show,Read,Typeable)

instance Bounded MPTimestamp where
  minBound = MPTimestamp minBound 0
  maxBound = MPTimestamp maxBound 999999999

instance NFData MPTimestamp where rnf (MPTimestamp _ _) = ()

-- | Construct 'MPTimestamp' from amount of integral seconds since Unix epoch
mptsFromPosixSeconds :: Int64 -> MPTimestamp
mptsFromPosixSeconds s = MPTimestamp s 0

-- | Construct 'MPTimestamp' from amount of seconds and nanoseconds (must be \( \leq 10^9 \) ) passed since Unix epoch
mptsFromPosixSeconds2 :: Int64 -> Word32 -> Maybe MPTimestamp
mptsFromPosixSeconds2 s ns
  | ns <= 999999999 = Just $! MPTimestamp s ns
  | otherwise       = Nothing

-- | Deconstruct 'MPTimestamp' into amount of seconds and nanoseconds passed since Unix epoch
mptsToPosixSeconds2 :: MPTimestamp -> (Int64, Word32)
mptsToPosixSeconds2 (MPTimestamp s ns) = (s, ns)

-- | Construct 'MPTimestamp' from total amount of nanoseconds passed since Unix epoch
mptsFromPosixNanoseconds :: Integer -> Maybe MPTimestamp
mptsFromPosixNanoseconds ns0
  | minI <= ns0, ns0 <= maxI  = Just $! MPTimestamp (fromInteger s) (fromInteger ns)
  | otherwise                 = Nothing
  where
    (s,ns) = divMod ns0 1000000000
    maxI = mptsToPosixNanoseconds maxBound
    minI = mptsToPosixNanoseconds minBound

-- | Deconstruct 'MPTimestamp' into total amount of nanoseconds passed since Unix epoch
mptsToPosixNanoseconds :: MPTimestamp -> Integer
mptsToPosixNanoseconds (MPTimestamp s ns) = (toInteger s * 1000000000) + toInteger ns

-- >>> mptsToUTCTime minBound
-- -292277022657-01-27 08:29:52 UTC

-- >>> mptsToUTCTime maxBound
-- 292277026596-12-04 15:30:07.999999999 UTC

-- >>> mptsToUTCTime (MPTimestamp 0 0)
-- 1970-01-01 00:00:00 UTC

-- >>> mptsToUTCTime (MPTimestamp 0xffffffff 0)
-- 2106-02-07 06:28:15 UTC

-- >>> mptsToUTCTime (MPTimestamp 0x3ffffffff 999999999)
-- 2514-05-30 01:53:03.999999999 UTC

-- | Convert 'MPTimestamp' into 'Time.UTCTime'
mptsToUTCTime :: MPTimestamp -> Time.UTCTime
mptsToUTCTime = picoseconds2utc . (*1000) . mptsToPosixNanoseconds

-- >>> mptsFromUTCTime (mptsToUTCTime minBound) == Just minBound
-- True

-- >>> mptsFromUTCTime (mptsToUTCTime maxBound) == Just maxBound
-- True

utc2picoseconds :: Time.UTCTime -> Integer
utc2picoseconds utc = ps
  where -- NB: this exploits the RULE from time:
    -- "realToFrac/NominalDiffTime->Pico"       realToFrac = \(MkNominalDiffTime ps) -> ps
    MkFixed ps = realToFrac (Time.utcTimeToPOSIXSeconds utc) :: Pico

-- NB: exploits the RULE
-- "realToFrac/Pico->NominalDiffTime"       realToFrac = MkNominalDiffTime
picoseconds2utc :: Integer -> Time.UTCTime
picoseconds2utc ps = Time.posixSecondsToUTCTime (realToFrac (MkFixed ps :: Pico))

-- | Convert 'Time.UTCTime' into 'MPTimestamp'
--
-- This conversion can fail (i.e. result in 'Nothing') if either the conversion cannot be performed lossless, either because the range of 'MPTimestamp' was exceeded or because of sub-nanosecond fractions.
--
-- See also 'mptsFromUTCTimeLossy'
mptsFromUTCTime :: Time.UTCTime -> Maybe MPTimestamp
mptsFromUTCTime t
  | rest /= 0 = Nothing
  | otherwise = mptsFromPosixNanoseconds ns0
  where
    (ns0,rest) = divMod (utc2picoseconds t) 1000

-- | Version of 'mptsFromUTCTime' which performs a lossy conversion into 'MPTimestamp'
--
-- * sub-nanosecond precision is silently truncated (in the sense of 'floor') to nanosecond precision
--
-- * time values exceeding the range of 'MPTimestamp' are clamped to 'minBound' and 'maxBound' respectively
--
mptsFromUTCTimeLossy :: Time.UTCTime -> MPTimestamp
mptsFromUTCTimeLossy t
  | Just mpts <- mptsFromPosixNanoseconds ns0 = mpts
  | ns0 < 0 = minBound
  | otherwise = maxBound
  where
    ns0 = div  (utc2picoseconds t) 1000

----------------------------------------------------------------------------

instance MessagePack MPTimestamp where
  toObject = ObjectExt XTAG_Timestamp . mptsEncode

  fromObject = \case
    ObjectExt XTAG_Timestamp bs -> mptsDecode bs
    obj                         -> typeMismatch "MPTimestamp" obj

-- helpers for 'MessagePack' instance
mptsEncode :: MPTimestamp -> S.ByteString
mptsEncode = runPut' . snd . mptsPutExtData

mptsDecode :: S.ByteString -> Result MPTimestamp
mptsDecode bs = do
  len <- maybe (fail "invalid data-length for Timestamp") pure $ intCastMaybe (S.length bs)
  either fail pure $ runGet' bs (mptsGetExtData len)

-- | This 'Binary' instance encodes\/decodes to\/from MessagePack format
instance Bin.Binary MPTimestamp where
  get = getExt' $ \typ sz -> do
          unless (typ == XTAG_Timestamp) $ fail "invalid extended type-tag for Timestamp"
          mptsGetExtData sz

  put = putExt' XTAG_Timestamp . mptsPutExtData

mptsPutExtData :: MPTimestamp -> (Word32,Bin.Put)
mptsPutExtData (MPTimestamp sec ns)
  | ns == 0, Just sec' <- intCastMaybe sec = (4, Bin.putWord32be sec')
  | 0 <= sec, sec <= 0x3ffffffff = (8, do
      let s' = ((intCast ns :: Word64) `shiftL` 34) .|. (fromIntegral sec)
      Bin.putWord64be s')
  | otherwise = (12, do
      Bin.putWord32be ns
      Bin.putInt64be sec)

mptsGetExtData :: Word32 -> Bin.Get MPTimestamp
mptsGetExtData = \case
  4 -> do
    s <- Bin.getWord32be
    pure $! MPTimestamp (intCast s) 0

  8 -> do
    dat <- Bin.getWord64be
    let s  = fromIntegral (dat .&. 0x3ffffffff)
        ns = fromIntegral (dat `shiftR` 34)
    when (ns > 999999999) $ fail "invalid nanosecond value"
    pure $! MPTimestamp s ns

  12 -> do
    ns <- Bin.getWord32be
    s  <- Bin.getInt64be
    when (ns > 999999999) $ fail "invalid nanosecond value"
    pure $! MPTimestamp s ns

  _ -> fail "unsupported timestamp encoding (length)"
