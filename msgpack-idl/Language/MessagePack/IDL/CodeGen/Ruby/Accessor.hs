{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Ruby.Accessor (
  genAccessors
  ) where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

data AccessorType = Read | ReadWrite deriving Eq

genAccessors :: [Maybe Field] -> LT.Text
genAccessors [] = ""
genAccessors fs = [lt|
#{genAccessors' Read "attr_reader" fs}#{genAccessors' ReadWrite "attr_accessor" fs}|]

genAccessors' :: AccessorType -> String -> [Maybe Field] -> LT.Text
genAccessors' at an flds = gen $ map (maybe undefined fldName) $ filter fldTypeEq flds
  where
    gen [] = ""
    gen fs = [lt|
#{an} #{T.intercalate ", " $ map (mappend ":") fs}|]

    fldTypeEq (Just Field {..}) = at == getAccessorType fldType
    fldTypeEq Nothing           = False

getAccessorType :: Type -> AccessorType
getAccessorType TBool = Read
getAccessorType (TMap _ _) = Read
getAccessorType (TUserDef _ _) = Read
getAccessorType _ = ReadWrite

