module Language.MessagePack.IDL.CodeGen.Cpp.Config where

data Config
  = Config
    { configFilePath :: FilePath
    , configNameSpace :: String
    , configPFICommon :: Bool
    }
  deriving (Show, Eq)
