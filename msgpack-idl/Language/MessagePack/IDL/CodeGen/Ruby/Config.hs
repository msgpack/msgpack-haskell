module Language.MessagePack.IDL.CodeGen.Ruby.Config where

data Config
  = Config
    { configFilePath :: FilePath
    , configModule :: String
    }
  deriving (Show, Eq)

