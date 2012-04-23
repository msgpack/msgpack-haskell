{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Python.Init (
  genInitMain
  ) where

import qualified Data.Text.Lazy.IO as LT
import Text.Shakespeare.Text

genInitMain :: IO ()
genInitMain = LT.writeFile "__init__.py" $ [lt|
|]
