{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java.Method where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax
import Language.MessagePack.IDL.CodeGen.Java.Util
import Language.MessagePack.IDL.CodeGen.Java.Decl

genMethodCall :: Method -> LT.Text
genMethodCall Function {..} =
  let args = T.intercalate ", " $ map genArgs' methodArgs
      vals = T.intercalate ", " $ pack methodArgs genVal in
  case methodRetType of
    TVoid -> [lt|
  public void #{methodName}(#{args}) {
    iface_.#{methodName}(#{vals});
  }
|]
    _ -> [lt|
  public #{genType methodRetType} #{methodName}(#{args}) {
    return iface_.#{methodName}(#{vals});
  }
|]
genMethodCall _ = ""

genSignature :: Method -> LT.Text
genSignature Function {..} = 
    [lt|    #{genType methodRetType} #{methodName}(#{args});
|]
    where
      args = (T.intercalate ", " $ map genArgs' methodArgs)
genSignature  _ = ""

genArgs' :: Field -> T.Text
genArgs' Field {..} = [st|#{genType fldType} #{fldName}|]

genVal :: Maybe Field -> T.Text
genVal Nothing = "null"
genVal (Just field) = fldName field

