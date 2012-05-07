{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Python.Client (
  genClientMain
  ) where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

import Language.MessagePack.IDL.CodeGen.Python.Util
import Language.MessagePack.IDL.CodeGen.Python.Type

genClientMain :: Spec -> IO ()
genClientMain spec = LT.writeFile "client.py" [lt|
import msgpackrpc
from types import *

#{LT.concat $ map (genClient) spec}
|]

genClient :: Decl -> LT.Text
genClient MPService {..} = [lt|
class #{serviceName}:
  def __init__ (self, host, port):
    address = msgpackrpc.Address(host, port)
    self.client = msgpackrpc.Client(address)
#{LT.concat $ map genMethodCall serviceMethods}
|]
  where
  genMethodCall Function {..} =
    let arg_list = map (maybe undefined fldName) $ sortField methodArgs
        args = LT.concat $ map (\x -> [lt|, #{x}|]) arg_list
    in
    case methodRetType of
      TVoid -> [lt|
  def #{methodName} (self#{args}):
    self.client.call('#{methodName}'#{args})
|]
      ts -> [lt|
  def #{methodName} (self#{args}):
    retval = self.client.call('#{methodName}'#{args})
    return #{fromMsgpack ts "retval"}
|]

  genMethodCall _ = ""

genClient _ = ""

