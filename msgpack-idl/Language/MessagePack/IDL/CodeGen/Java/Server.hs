{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java.Server (
  genServer
  ) where

import qualified Data.Text.Lazy as LT

import Language.MessagePack.IDL.Syntax

genServer :: Decl -> LT.Text
genServer _ = ""

