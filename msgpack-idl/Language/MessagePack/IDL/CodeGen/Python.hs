{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Python (
  Config(..),
  generate,
  ) where

import System.FilePath
import System.Directory

import Language.MessagePack.IDL.Syntax

import Language.MessagePack.IDL.CodeGen.Python.Client
import Language.MessagePack.IDL.CodeGen.Python.Config
import Language.MessagePack.IDL.CodeGen.Python.Init
import Language.MessagePack.IDL.CodeGen.Python.Server
import Language.MessagePack.IDL.CodeGen.Python.TypeDecl

generate:: Config -> Spec -> IO ()
generate config spec = do
  createDirectoryIfMissing True (takeBaseName $ configFilePath config);
  setCurrentDirectory (takeBaseName $ configFilePath config);
  genInitMain
  genTypeDeclMain config spec
  genServerMain
  genClientMain spec

