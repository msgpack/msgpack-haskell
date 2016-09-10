{-# LANGUAGE Safe #-}
--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack
-- Copyright : (c) Hideyuki Tanaka, 2009-2015
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- Simple interface to pack and unpack MessagePack data.
--
--------------------------------------------------------------------

module Data.MessagePack
  -- * Simple interface to pack and unpack msgpack binary
  ( pack
  , unpack

  -- * Re-export modules
  -- $reexports
  , module X
  ) where

import           Control.Applicative      (Applicative)
import           Control.Monad            ((>=>))
import           Data.Binary              (decodeOrFail, encode)
import qualified Data.ByteString.Lazy     as L

import           Data.MessagePack.Assoc   as X
import           Data.MessagePack.Class   as X
import           Data.MessagePack.Generic ()
import           Data.MessagePack.Get     as X
import           Data.MessagePack.Object  as X
import           Data.MessagePack.Put     as X


-- | Pack a Haskell value to MessagePack binary.
pack :: MessagePack a => a -> L.ByteString
pack = encode . toObject

-- | Unpack MessagePack binary to a Haskell value. If it fails, it fails in the
-- Monad. In the Maybe monad, failure returns Nothing.
unpack :: (Applicative m, Monad m, MessagePack a)
       => L.ByteString -> m a
unpack = eitherToM . decodeOrFail >=> fromObject
  where
    eitherToM (Left  (_, _, msg)) = fail msg
    eitherToM (Right (_, _, res)) = return res
