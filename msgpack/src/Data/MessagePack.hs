--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack
-- Copyright : © Hideyuki Tanaka 2009-2015
--           , © Herbert Valerio Riedel 2019
-- License   : BSD3
--
-- Simple interface to encode\/decode to\/from the [MessagePack](https://msgpack.org/) format.
--
--
--------------------------------------------------------------------

module Data.MessagePack (
  -- * Simple interface to pack and unpack msgpack binary
  pack, unpack,

  -- * Re-export modules
  -- $reexports
  module Data.MessagePack.Assoc,
  module Data.MessagePack.Get,
  module Data.MessagePack.Object,
  module Data.MessagePack.Put,
  ) where

import           Data.Binary             (decode, encode)
import qualified Data.ByteString.Lazy    as L

import           Data.MessagePack.Assoc
import           Data.MessagePack.Get
import           Data.MessagePack.Object
import           Data.MessagePack.Put

-- | Pack a Haskell value to MessagePack binary.
pack :: MessagePack a => a -> L.ByteString
pack = encode . toObject

-- | Unpack MessagePack binary to a Haskell value. If it fails, it returns Nothing.
unpack :: MessagePack a => L.ByteString -> Maybe a
unpack = fromObject . decode