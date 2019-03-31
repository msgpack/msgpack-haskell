{-# LANGUAGE CPP                 #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
#endif

-- |
-- Module    : Data.MessagePack.Tags
-- Copyright : © Herbert Valerio Riedel 2019
-- License   : BSD3
--
-- The tag constants in this module were carefully copied from the table at
--
-- https://github.com/msgpack/msgpack/blob/master/spec.md#formats
--
module Data.MessagePack.Tags where

import           Compat.Prelude

-- | Test whether tag is a fixint
is_TAG_fixint :: Word8 -> Bool
is_TAG_fixint tag = (tag .&. TAG_MASK_fixintp == TAG_fixintp)
                 || (tag .&. TAG_MASK_fixintn == TAG_fixintn)
{-# INLINE is_TAG_fixint #-}

pattern TAG_fixintn       = 0xe0 -- 0b111xxxxx [0xe0 .. 0xff] / [-32 .. -1]
pattern TAG_MASK_fixintn  = 0xe0 -- 0b11100000

pattern TAG_fixintp       = 0x00 -- 0b0xxxxxxx [0x00 .. 0x7f] / [0 .. 127]
pattern TAG_MASK_fixintp  = 0x80 -- 0b10000000

-- | Test whether tag is a fixmap and return embedded-size if it is
is_TAG_fixmap :: Word8 -> Maybe Word32
is_TAG_fixmap t
  | t .&. TAG_MASK_fixmap == TAG_fixmap = Just $! intCast (t .&. complement TAG_MASK_fixmap)
  | otherwise                               = Nothing
{-# INLINE is_TAG_fixmap #-}

pattern TAG_fixmap        = 0x80 -- 0b1000xxxx [0x80 .. 0x8f]
pattern TAG_MASK_fixmap   = 0xf0 -- 0b11110000

-- | Test whether tag is a fixarray and return embedded-size if it is
is_TAG_fixarray :: Word8 -> Maybe Word32
is_TAG_fixarray t
  | t .&. TAG_MASK_fixarray == TAG_fixarray = Just $! intCast (t .&. complement TAG_MASK_fixarray)
  | otherwise                               = Nothing
{-# INLINE is_TAG_fixarray #-}

pattern TAG_fixarray      = 0x90 -- 0b1001xxxx [0x90 .. 0x9f]
pattern TAG_MASK_fixarray = 0xf0 -- 0b11110000

-- | Test whether tag is a fixstr and return embedded-size if it is
is_TAG_fixstr :: Word8 -> Maybe Word32
is_TAG_fixstr t
  | t .&. TAG_MASK_fixstr == TAG_fixstr = Just $! intCast (t .&. complement TAG_MASK_fixstr)
  | otherwise                           = Nothing
{-# INLINE is_TAG_fixstr #-}

pattern TAG_fixstr        = 0xa0 -- 0b101xxxxx [0xa0 .. 0xbf]
pattern TAG_MASK_fixstr   = 0xe0 -- 0b11100000

pattern TAG_nil           = 0xc0 -- 0b11000000
pattern TAG_reserved_C1   = 0xc1 -- 0b11000001
pattern TAG_false         = 0xc2 -- 0b11000010
pattern TAG_true          = 0xc3 -- 0b11000011

pattern TAG_bin8          = 0xc4 -- 0b11000100
pattern TAG_bin16         = 0xc5 -- 0b11000101
pattern TAG_bin32         = 0xc6 -- 0b11000110

pattern TAG_ext8          = 0xc7 -- 0b11000111
pattern TAG_ext16         = 0xc8 -- 0b11001000
pattern TAG_ext32         = 0xc9 -- 0b11001001

pattern TAG_float32       = 0xca -- 0b11001010
pattern TAG_float64       = 0xcb -- 0b11001011

pattern TAG_uint8         = 0xcc -- 0b11001100
pattern TAG_uint16        = 0xcd -- 0b11001101
pattern TAG_uint32        = 0xce -- 0b11001110
pattern TAG_uint64        = 0xcf -- 0b11001111

pattern TAG_int8          = 0xd0 -- 0b11010000
pattern TAG_int16         = 0xd1 -- 0b11010001
pattern TAG_int32         = 0xd2 -- 0b11010010
pattern TAG_int64         = 0xd3 -- 0b11010011

pattern TAG_fixext1       = 0xd4 -- 0b11010100
pattern TAG_fixext2       = 0xd5 -- 0b11010101
pattern TAG_fixext4       = 0xd6 -- 0b11010110
pattern TAG_fixext8       = 0xd7 -- 0b11010111
pattern TAG_fixext16      = 0xd8 -- 0b11011000

pattern TAG_str8          = 0xd9 -- 0b11011001
pattern TAG_str16         = 0xda -- 0b11011010
pattern TAG_str32         = 0xdb -- 0b11011011

pattern TAG_array16       = 0xdc -- 0b11011100
pattern TAG_array32       = 0xdd -- 0b11011101

pattern TAG_map16         = 0xde -- 0b11011110
pattern TAG_map32         = 0xdf -- 0b11011111

-- NOTE: Currently the MessagePack specification only defines the @-1@
-- extension type (for timestamps). All remaining negative Int8
-- type-ids are reserved for future use by the MessagePack.

-- Used by "Data.MessagePack.Timestamp"
pattern XTAG_Timestamp = -1 :: Int8
