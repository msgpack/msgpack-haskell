{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Safe          #-}
module Data.MessagePack.Result where

import           Control.Applicative       (Applicative, pure, (<$>), (<*>))
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
import qualified Test.QuickCheck.Gen       as Gen


data Result a
  = Success a
  | Failure String
  deriving (Read, Show, Eq, Functor)


instance Applicative Result where
  pure = Success

  Success f   <*> x = fmap f x
  Failure msg <*> _ = Failure msg


instance Monad Result where
  return = pure
  fail = Failure

  Success x   >>= f = f x
  Failure msg >>= _ = Failure msg


instance Arbitrary a => Arbitrary (Result a) where
  arbitrary = Gen.oneof
    [ Success <$> arbitrary
    , Failure <$> arbitrary
    ]
