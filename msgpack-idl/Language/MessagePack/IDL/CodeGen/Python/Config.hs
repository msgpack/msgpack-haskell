module Language.MessagePack.IDL.CodeGen.Python.Config where

data Config
  = Config
    { configFilePath :: FilePath }
  deriving (Show, Eq)

