{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Py (
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

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} spec = do
  let name = takeBaseName configFilePath

      typeHeader = [lt|import msgpack|]
      serverHeader = [lt|import msgpackrpc|]
      clientHeader = [lt|import msgpackrpc|]
  
  LT.writeFile (name ++ "_types.py") $ templ "TYPES" [lt|
import sys
#{typeHeader}

#{LT.concat $ map (genTypeDecl name) spec }
|]

  LT.writeFile (name ++ "_server.py") $ templ "SERVER" [lt|
import #{name}_types
#{serverHeader}
|]

  LT.writeFile (name ++ "_client.py") [lt|
import #{name}_types
#{clientHeader}
#{LT.concat $ map (genClient) spec}
|]

genTypeDecl :: String -> Decl -> LT.Text

genTypeDecl _ MPMessage {..} =
  genMsg msgName msgFields False

genTypeDecl _ MPException {..} =
  genMsg excName excFields True

genTypeDecl _ _ = ""

genMsg name flds isExc =
  let fields = map f flds
      types = map typ flds
      fs = map (maybe undefined fldName) $ sortField flds
  in [lt|
class #{name}#{e}:

  def __init__(self#{LT.concat $ map g fs}):
#{LT.concat fields}
  def to_array(self):
    variables = []
#{LT.concat types}
    return variables
|]

  where
    e = if isExc then [lt|(Exception)|] else ""
    f Field {..} = [lt|    self.#{fldName} = #{fldName}
|]
    typ Field {..} = let selfName = "self." `mappend` fldName
                     in [lt|    variables.append(#{genType fldType selfName})
|]
    g str = [lt|, #{str}|]

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
    let arg_list = map (maybe undefined fldName) $ sortField methodArgs
        args = LT.concat $ map arg arg_list
        to_arrays = LT.concat $ map to_array methodArgs in
    case methodRetType of
      TVoid -> [lt|
  def #{methodName} (self#{args}):
#{to_arrays}
    self.client.call('#{methodName}'#{args})
|]
      _ -> [lt|
  def #{methodName} (self#{args}):
#{to_arrays}
    return self.client.call('#{methodName}'#{args})
|]
    where
      arg str = [lt|, #{str}|]
      to_array Field {..} = [lt|    #{fldName} = #{genType fldType fldName}
|]

  genMethodCall _ = ""

genClient _ = ""

genType :: Type -> T.Text -> LT.Text
genType (TInt sign bits) name = [lt|#{name}|]
genType (TFloat False) name = [lt|#{name}|]
genType (TFloat True) name = [lt|#{name}|]
genType TBool name = [lt|#{name}|]
genType TRaw name = [lt|#{name}|]
genType TString name = [lt|#{name}|]
genType (TList typ) name =
  [lt|[#{genType typ "elem"} for elem in #{name}]|]
genType (TMap typ1 typ2) name =
  [lt|{#{genType typ1 "k"} : #{genType typ2 "v"} for k,v in #{name}.items()}|]
genType (TUserDef className params) name = [lt|#{name}.to_array()|]
genType (TTuple ts) name = [lt|#{name}.to_array()|]
  -- TODO: FIX
  -- foldr1 (\t1 t2 -> [lt|std::pair<#{t1}, #{t2} >|]) $ map genType ts
genType TObject name = [lt|#{name}.to_array()|]
genType TVoid name = ""


templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|
#/usr/bin/python
# -*- coding:utf-8 -*-

# This file is auto-generated from #{filepath}
# *** DO NOT EDIT ***

#{content}
|]