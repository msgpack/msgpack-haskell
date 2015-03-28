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

module Data.MessagePack (
  -- * Simple interface to pack and unpack msgpack binary
  pack, unpack,

  -- * Re-export modules
  -- $reexports
  -- module X,
  module Data.MessagePack.Assoc,
  module Data.MessagePack.Get,
  module Data.MessagePack.Object,
  module Data.MessagePack.Put,
  ) where

import           Data.Binary
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
