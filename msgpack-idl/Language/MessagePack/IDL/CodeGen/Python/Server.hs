{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Python.Server (
  genServerMain
  ) where

import qualified Data.Text.Lazy.IO as LT
import Text.Shakespeare.Text

genServerMain :: IO ()
genServerMain =   LT.writeFile "server.tmpl.py" $ [lt|
import msgpackrpc
from types import *
# write your server here and change file name to server.py

|]

