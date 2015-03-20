{-# LANGUAGE TypeOperators #-}

module Data.MessagePack.Generic (
  GPackable(..),
  ) where

import           Blaze.ByteString.Builder
import           Data.Monoid
import           GHC.Generics

packUnit = undefined

class GPackable f where
  gfrom :: f a -> Builder

instance GPackable U1 where
  gfrom _ = pack ()
  {-# INLINE gfrom #-}

instance (GPackable a, GPackable b) => GPackable (a :*: b) where
  gfrom (a :*: b) = gfrom a <> gfrom b
  {-# INLINE gfrom #-}

instance (GPackable a, GPackable b) => GPackable (a :+: b) where
  gfrom (L1 x) = gfrom x
  gfrom (R1 x) = gfrom x
  {-# INLINE gfrom #-}

instance GPackable a => GPackable (D1 c a) where
  gfrom (M1 x) = from x
  {-# INLINE gfrom #-}

instance GPackable a => GPackable (C1 c a) where
  gfrom (M1 x) = undefined
  {-# INLINE gfrom #-}

instance Packable a => GPackable (K1 i a) where
  gfrom (K1 x) = from x
  {-# INLINE gfrom #-}
