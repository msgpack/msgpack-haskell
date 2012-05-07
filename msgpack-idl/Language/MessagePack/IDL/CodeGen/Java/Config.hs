module Language.MessagePack.IDL.CodeGen.Java.Config where

data Config
  = Config
    { configFilePath :: FilePath
    , configPackage :: String
    }
  deriving (Show, Eq)

