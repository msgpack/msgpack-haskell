{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java (
  Config(..),
  generate,
  ) where

import Language.MessagePack.IDL.Syntax

import Language.MessagePack.IDL.CodeGen.Java.Client
import Language.MessagePack.IDL.CodeGen.Java.Struct
import Language.MessagePack.IDL.CodeGen.Java.Exception
import Language.MessagePack.IDL.CodeGen.Java.Tuple
import Language.MessagePack.IDL.CodeGen.Java.Config
import Language.MessagePack.IDL.CodeGen.Java.Alias

generate :: Config -> Spec -> IO()
generate config spec = do
  let typeAlias = map genAlias $ filter isMPType spec

  genTuple config
  mapM_ (genClient typeAlias config) spec
  mapM_ (genStruct typeAlias $ configPackage config) spec
  mapM_ (genException $ configPackage config) spec

