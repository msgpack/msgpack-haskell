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
  module X,

  -- * Simple interface to pack and unpack msgpack binary
  pack, unpack,
  ) where

import           Data.Binary
import qualified Data.ByteString.Lazy    as L

import           Data.MessagePack.Assoc  as X
import           Data.MessagePack.Get    as X
import           Data.MessagePack.Object as X
import           Data.MessagePack.Put    as X

pack :: MessagePack a => a -> L.ByteString
pack = encode . toObject

unpack :: MessagePack a => L.ByteString -> Maybe a
unpack = fromObject . decode
