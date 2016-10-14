{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.MessagePack.Instances () where

import           Data.Void                (Void)

import           Data.MessagePack.Class   (MessagePack)
import           Data.MessagePack.Generic ()


instance MessagePack a => MessagePack (Maybe a)

instance (MessagePack a, MessagePack b) => MessagePack (Either a b)

instance MessagePack Void
