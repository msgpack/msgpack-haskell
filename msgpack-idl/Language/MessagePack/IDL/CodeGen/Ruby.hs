{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Ruby (
  Config(..),
  generate,
  ) where

import Data.Char
import qualified Data.Text.Lazy as LT
import System.FilePath

import Language.MessagePack.IDL.Syntax
import Language.MessagePack.IDL.CodeGen.Ruby.Config
import Language.MessagePack.IDL.CodeGen.Ruby.Client
import Language.MessagePack.IDL.CodeGen.Ruby.TypeDecl

generate:: Config -> Spec -> IO ()
generate config spec = do
  let name = takeBaseName $ configFilePath config
      once = map toUpper name
      mods = LT.splitOn "::" $ LT.pack $ configModule config
  genTypeDeclMain name once mods config spec
  genClientMain name once mods config spec

