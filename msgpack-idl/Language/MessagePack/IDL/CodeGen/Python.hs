{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Python (
  Config(..),
  generate,
  ) where

import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text
import System.Directory

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} spec = do
  createDirectoryIfMissing True (takeBaseName configFilePath);
  setCurrentDirectory (takeBaseName configFilePath);
  LT.writeFile "__init__.py" $ templ configFilePath [lt|
|]
  LT.writeFile "types.py" $ templ configFilePath [lt|
import sys
import msgpack

#{LT.concat $ map (genTypeDecl "") spec }
|]

  LT.writeFile "server.tmpl.py" $ templ configFilePath [lt|
import msgpackrpc
from types import *
# write your server here and change file name to server.py

|]

  LT.writeFile "client.py" $ templ configFilePath [lt|
import msgpackrpc
from types import *

#{LT.concat $ map (genClient) spec}
|]

genTypeDecl :: String -> Decl -> LT.Text

genTypeDecl _ MPType {..} = [lt|
class #{tyName}:
  @staticmethod
  def from_msgpack(arg):
    return #{fromMsgpack tyType "arg"}
|]

genTypeDecl _ MPMessage {..} =
  genMsg msgName msgFields False

genTypeDecl _ MPException {..} =
  genMsg excName excFields True

genTypeDecl _ _ = ""

genMsg :: ToText a => a -> [Field] -> Bool -> LT.Text
genMsg name flds isExc =
  let fs = zipWith (\ix -> maybe ("_UNUSED" `mappend` T.pack (show ix)) fldName) [0 .. ] (sortField flds)
  in [lt|
class #{name}#{e}:
  def __init__(self, #{LT.intercalate ", " $ map g fs}):
#{LT.concat $ map f flds}
  def to_msgpack(self):
    return (#{LT.concat $ map typ flds}
      )

  @staticmethod
  def from_msgpack(arg):
    return #{name}(
      #{LT.intercalate ",\n      " $ map make_arg flds})
|]

  where
    e = if isExc then [lt|(Exception)|] else ""
    f Field {..} = [lt|    self.#{fldName} = #{fldName}
|]
    typ Field {..} = [lt|
      self.#{fldName},|]
    make_arg Field {..} =
      let fldId_str = T.concat $ map T.pack ["arg[", (show fldId), "]"] in
      [lt|#{fromMsgpack fldType fldId_str}|]
    g str = [lt|#{str}|]

sortField :: [Field] -> [Maybe Field]
sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix ->
  find ((==ix). fldId) flds

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
    let arg_list = zipWith (\ix -> maybe ("_UNUSED" `mappend` T.pack (show ix)) fldName) [0 .. ] $ sortField methodArgs
        args = LT.concat $ map (\x -> [lt|, #{x}|]) arg_list
    in
    case methodRetType of
      Nothing -> [lt|
  def #{methodName} (self#{args}):
    self.client.call('#{methodName}'#{args})
|]
      Just ts -> [lt|
  def #{methodName} (self#{args}):
    retval = self.client.call('#{methodName}'#{args})
    return #{fromMsgpack ts "retval"}
|]

  genMethodCall _ = ""

genClient _ = ""

sanitize :: Char -> Char
sanitize '[' = '_'
sanitize ']' = '_'
sanitize c = c

fromMsgpack :: Type -> T.Text -> LT.Text
fromMsgpack (TNullable t) name = fromMsgpack t name
fromMsgpack (TInt _ _) name = [lt|#{name}|]
fromMsgpack (TFloat False) name = [lt|#{name}|]
fromMsgpack (TFloat True) name = [lt|#{name}|]
fromMsgpack TBool name = [lt|#{name}|]
fromMsgpack TRaw name = [lt|#{name}|]
fromMsgpack TString name = [lt|#{name}|]
fromMsgpack (TList typ) name =
  let
    varname = T.append (T.pack "elem_") (T.map sanitize name) in
  [lt|[#{fromMsgpack typ varname} for #{varname} in #{name}]|]

fromMsgpack (TMap typ1 typ2) name =
  let
    keyname = T.append (T.pack "k_" ) $ T.map sanitize name
    valname = T.append (T.pack "v_" ) $ T.map sanitize name
  in
  [lt|{#{fromMsgpack typ1 keyname} : #{fromMsgpack typ2 valname} for #{keyname},#{valname} in #{name}.items()}|]

fromMsgpack (TUserDef className _) name = [lt|#{className}.from_msgpack(#{name})|]
            
fromMsgpack (TTuple ts) name =
            let elems = map (f name) (zip [0..] ts) in
            [lt| (#{LT.intercalate ", " elems}) |]
            where
              f :: T.Text -> (Integer, Type) -> LT.Text
              f n (i, (TUserDef className _ )) = [lt|#{className}.from_msgpack(#{n}[#{show i}]) |]
              f n (i, _) = [lt|#{n}[#{show i}]|]

fromMsgpack TObject name = [lt|#{name}|]

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|
# This file is auto-generated from #{filepath}
# *** DO NOT EDIT ***

#{content}
|]
