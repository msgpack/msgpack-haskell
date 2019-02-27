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
  -- ** Lazy 'L.ByteString'
  pack, unpack,

  -- ** Strict 'L.ByteString'
  pack', unpack',

  -- * Re-export modules
  module Data.MessagePack.Assoc,
  module Data.MessagePack.Get,
  module Data.MessagePack.Object,
  module Data.MessagePack.Put,
  ) where

import           Compat.Binary           (get, runGet, runGet', runPut, runPut')
import qualified Data.ByteString         as S
import qualified Data.ByteString.Lazy    as L

import           Data.MessagePack.Assoc
import           Data.MessagePack.Get
import           Data.MessagePack.Object
import           Data.MessagePack.Put

-- | Pack a Haskell value to MessagePack binary.
pack :: MessagePack a => a -> L.ByteString
pack = runPut . toBinary

-- | Unpack MessagePack binary to a Haskell value. If it fails, it returns 'Left' with an error message.
--
-- @since 1.1.0.0
unpack :: MessagePack a => L.ByteString -> Either String a
unpack bs = do
  obj <- runGet bs get
  case fromObject obj of
    Success a -> Right a
    Error e   -> Left e


-- | Variant of 'pack' serializing to a strict 'ByteString'
--
-- @since 1.1.0.0
pack' :: MessagePack a => a -> S.ByteString
pack' = runPut' . toBinary

-- | Variant of 'unpack' serializing to a strict 'ByteString'
--
-- @since 1.1.0.0
unpack' :: MessagePack a => S.ByteString -> Either String a
unpack' bs = do
  obj <- runGet' bs get
  case fromObject obj of
    Success a -> Right a
    Error e   -> Left e
