{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Python.TypeDecl (
  genTypeDeclMain
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

import Language.MessagePack.IDL.CodeGen.Python.Util
import Language.MessagePack.IDL.CodeGen.Python.Type
import Language.MessagePack.IDL.CodeGen.Python.Config

genTypeDeclMain :: Config -> Spec -> IO ()
genTypeDeclMain Config {..} spec = LT.writeFile "types.py" $ templ configFilePath [lt|
import sys
import msgpack

#{LT.concat $ map (genTypeDecl "") spec }
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
  let
      fs = map (maybe undefined fldName) $ sortField flds
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

