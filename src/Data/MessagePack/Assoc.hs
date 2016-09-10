{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy                #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Assoc
-- Copyright : (c) Daiki Handa, 2010-2011
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- MessagePack map labeling type
--
--------------------------------------------------------------------

module Data.MessagePack.Assoc
  ( Assoc (..)
  ) where

import           Control.Applicative       ((<$>))
import           Control.DeepSeq           (NFData)
import           Data.Typeable             (Typeable)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

-- not defined for general Functor for performance reason.
-- (ie. you would want to write custom instances for each type using specialized mapM-like functions)
newtype Assoc a
  = Assoc { unAssoc :: a }
  deriving (Show, Read, Eq, Ord, Typeable, NFData)

instance Arbitrary a => Arbitrary (Assoc a) where
  arbitrary = Assoc <$> arbitrary
