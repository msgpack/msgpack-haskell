{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}

-- |
-- Module    : Data.MessagePack.Integer
-- Copyright : © Herbert Valerio Riedel 2019
-- License   : BSD3
--
-- Type representing MessagePack integers
--
-- @since 1.1.0.0
module Data.MessagePack.Result
    ( Result(..)
    ) where

import           Compat.Prelude
import qualified Control.Monad.Fail as Fail

-- | The result of decoding from MessagePack
--
-- @since 1.1.0.0
data Result a = Error String
              | Success a
              deriving (Eq, Show, Functor, Typeable, Generic, Foldable, Traversable)

instance NFData a => NFData (Result a) where
  rnf (Error e)   = rnf e
  rnf (Success a) = rnf a

instance Applicative Result where
    pure = Success
    (<*>) = ap

instance Monad Result where
    Success a >>= m = m a
    Error err >>= _ = Error err

#if !MIN_VERSION_base(4,13,0)
    return = pure
    fail = Fail.fail
#endif

instance Fail.MonadFail Result where
    fail = Error

instance Alternative Result where
    empty = fail "Alternative(empty)"
    a@(Success _) <|> _ = a
    _              <|> b = b
