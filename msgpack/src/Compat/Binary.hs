{-# LANGUAGE FlexibleContexts #-}

-- | Compat layer for "Data.Binary"
--
-- Supports @binary-0.7.1@ and later
module Compat.Binary
    ( Binary(put, get)

    , runPut', Bin.runPut, Bin.PutM, Put
    , runGet', runGet, Get

    , Bin.getWord64be, Bin.putWord64be
    , Bin.getWord32be, Bin.putWord32be
    , Bin.getWord16be, Bin.putWord16be
    , Bin.getWord8   , Bin.putWord8

    , getInt64be, putInt64be
    , getInt32be, putInt32be
    , getInt16be, putInt16be
    , getInt8   , putInt8

    , getFloat32be, putFloat32be
    , getFloat64be, putFloat64be

    , Bin.getByteString, Bin.putByteString
    ) where

import           Compat.Prelude

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL

import           Data.Array.ST        (MArray, STUArray, newArray, readArray)
import           Data.Array.Unsafe    (castSTUArray)
import           Data.Binary          (Binary (get, put), Get, Put)
import qualified Data.Binary.Get      as Bin
import qualified Data.Binary.Put      as Bin
import           GHC.ST               (ST, runST)


runGet' :: BS.ByteString -> Get a -> Either String a
runGet' bs0 g = case Bin.pushEndOfInput (Bin.runGetIncremental g `Bin.pushChunk` bs0) of
                  Bin.Done bs ofs x
                    | BS.null bs -> Right x
                    | otherwise -> Left ("unexpected trailing data (ofs="++show ofs++")")
                  Bin.Partial _ -> Left "truncated data"
                  Bin.Fail _ ofs e -> Left (e ++ " (ofs=" ++ show ofs ++ ")")

runPut' :: Put -> BS.ByteString
runPut' = BL.toStrict . Bin.runPut

runGet :: BL.ByteString -> Get a -> Either String a
runGet bs0 g = case Bin.runGetOrFail g bs0 of
                 Left (_,ofs,e) -> Left (e ++ " (ofs=" ++ show ofs ++ ")")
                 Right (bs,ofs,x)
                   | BL.null bs -> Right x
                   | otherwise -> Left ("unexpected trailing data (ofs="++show ofs++")")

-- NB: once we drop support for binary < 0.8.1 we can drop the ops below

{-# INLINE getInt8 #-}
getInt8 :: Get Int8
getInt8 = intCastIso <$> Bin.getWord8

{-# INLINE getInt16be #-}
getInt16be :: Get Int16
getInt16be = intCastIso <$> Bin.getWord16be

{-# INLINE getInt32be #-}
getInt32be :: Get Int32
getInt32be = intCastIso <$> Bin.getWord32be

{-# INLINE getInt64be #-}
getInt64be :: Get Int64
getInt64be = intCastIso <$> Bin.getWord64be

{-# INLINE putInt8 #-}
putInt8 :: Int8 -> Put
putInt8 x = Bin.putWord8 (intCastIso x)

{-# INLINE putInt16be #-}
putInt16be :: Int16 -> Put
putInt16be x = Bin.putWord16be (intCastIso x)

{-# INLINE putInt32be #-}
putInt32be :: Int32 -> Put
putInt32be x = Bin.putWord32be (intCastIso x)

{-# INLINE putInt64be #-}
putInt64be :: Int64 -> Put
putInt64be x = Bin.putWord64be (intCastIso x)

-- NB: Once we drop support for binary < 0.8.4 we can use @binary@'s own {get,put}{Double,Float}be operations

putFloat32be :: Float -> Put
putFloat32be x = Bin.putWord32be (runST (cast x))

putFloat64be :: Double -> Put
putFloat64be x = Bin.putWord64be (runST (cast x))

getFloat32be :: Get Float
getFloat32be = do
  x <- Bin.getWord32be
  return (runST (cast x))

getFloat64be :: Get Double
getFloat64be = do
  x <- Bin.getWord64be
  return (runST (cast x))

-- See https://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa/7002812#7002812

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
