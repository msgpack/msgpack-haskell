{-# LANGUAGE FlexibleContexts #-}

-- | Compat layer for "Data.Binary"
--
-- Supports @binary-0.7.1@ and later
module Compat.Binary
    ( Binary(put, get)

    , runPut', runPut, PutM, Put
    , runGet', runGet, Get

    , getWord64be, putWord64be
    , getWord32be, putWord32be
    , getWord16be, putWord16be
    , getWord8   , putWord8

    , getFloat32be, putFloat32be
    , getFloat64be, putFloat64be

    , getByteString, putByteString

      -- convenience
    , Data.Word.Word, Word8, Word16, Word32, Word64
    , Data.Int.Int, Int8, Int16, Int32, Int64
    ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL

import           Data.Array.ST        (MArray, STUArray, newArray, readArray)
import           Data.Array.Unsafe    (castSTUArray)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Int
import           Data.Word
import           GHC.ST               (ST, runST)


runGet' :: BS.ByteString -> Get a -> Maybe a
runGet' bs0 g = case pushEndOfInput (runGetIncremental g `pushChunk` bs0) of
                  Done bs _ x
                    | BS.null bs -> return x
                    | otherwise -> fail "trailing data"
                  Partial _ -> fail "eof"
                  Fail _ _ msg -> fail msg

runPut' :: Put -> BS.ByteString
runPut' = BL.toStrict . runPut


-- NB: Once we drop support for binary < 0.8.4 we can use @binary@'s own {get,put}{Double,Float}be operations

putFloat32be :: Float -> Put
putFloat32be x = putWord32be (runST (cast x))

putFloat64be :: Double -> Put
putFloat64be x = putWord64be (runST (cast x))

getFloat32be :: Get Float
getFloat32be = do
  x <- getWord32be
  return (runST (cast x))

getFloat64be :: Get Double
getFloat64be = do
  x <- getWord64be
  return (runST (cast x))

-- See https://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa/7002812#7002812

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
