{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Get
-- Copyright : © Hideyuki Tanaka 2009-2015
--           , © Herbert Valerio Riedel 2019
-- License   : BSD3
--
-- MessagePack Deserializer using "Data.Binary"
--
--------------------------------------------------------------------

module Data.MessagePack.Get
  ( getNil
  , getBool

  , getFloat
  , getDouble

  , getInt
  , getWord
  , getInt64
  , getWord64

  , getStr
  , getBin

  , getArray
  , getMap

  , getExt
  , getExt'
  ) where

import           Compat.Binary
import           Compat.Prelude
import           Data.MessagePack.Get.Internal
import           Data.MessagePack.Integer

-- | Deserialize an integer into an 'Int'
--
-- This operation will fail if the encoded integer doesn't fit into the value range of the 'Int' type.
--
-- @since 1.1.0.0
getInt :: Get Int
getInt = maybe empty pure =<< fromMPInteger <$> get

-- | Deserialize an integer into a 'Word'
--
-- This operation will fail if the encoded integer doesn't fit into the value range of the 'Word' type.
--
-- @since 1.0.1.0
getWord :: Get Word
getWord = maybe empty pure =<< fromMPInteger <$> get

-- | Deserialize an integer into an 'Int64'
--
-- This operation will fail if the encoded integer doesn't fit into the value range of the 'Int64' type.
--
-- @since 1.0.1.0
getInt64 :: Get Int64
getInt64 = maybe empty pure =<< fromMPInteger <$> get

-- | Deserialize an integer into a 'Word'
--
-- This operation will fail if the encoded integer doesn't fit into the value range of the 'Word64' type.
--
-- @since 1.0.1.0
getWord64 :: Get Word64
getWord64 = maybe empty pure =<< fromMPInteger <$> get

