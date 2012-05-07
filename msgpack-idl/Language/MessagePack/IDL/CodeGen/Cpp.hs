{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Cpp (
  Config(..),
  generate
  ) where

import Language.MessagePack.IDL.Syntax
import Language.MessagePack.IDL.CodeGen.Cpp.Config
import Language.MessagePack.IDL.CodeGen.Cpp.Server
import Language.MessagePack.IDL.CodeGen.Cpp.Client
import Language.MessagePack.IDL.CodeGen.Cpp.TypeDecl

generate:: Config -> Spec -> IO ()
generate config spec = do
  genTypeDeclMain config spec
  genServerMain   config spec
  genClientMain   config spec

