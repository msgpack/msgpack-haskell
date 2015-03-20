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

  pack, unpack,
  ) where

import           Data.Binary
import qualified Data.ByteString.Lazy    as L
import           Data.Maybe

import           Data.MessagePack.Assoc  as X
import           Data.MessagePack.Object as X
import           Data.MessagePack.Pack   as X
import           Data.MessagePack.Unpack as X

pack :: Msgpack a => a -> L.ByteString
pack = encode . toObject

-- FIXME
unpack :: Msgpack a => L.ByteString -> a
unpack = fromJust . fromObject . decode
