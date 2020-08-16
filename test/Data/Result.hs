{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Safe          #-}
module Data.Result
    ( Result (..)
    ) where

import           Control.Applicative (Applicative (..), (<$>), (<*>))
import           Control.Monad.Fail  (MonadFail (..))

data Result a
    = Success a
    | Failure String
    deriving (Read, Show, Eq, Functor)

instance Applicative Result where
    pure = Success

    Success f   <*> x = fmap f x
    Failure msg <*> _ = Failure msg

instance Monad Result where
    return = Success
#if !MIN_VERSION_base(4,13,0)
    fail = Failure
#endif

    Success x   >>= f = f x
    Failure msg >>= _ = Failure msg

instance MonadFail Result where
    fail = Failure
