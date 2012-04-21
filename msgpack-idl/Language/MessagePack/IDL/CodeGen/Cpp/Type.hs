{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Cpp.Type (
  genType
  ) where

import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

genType :: Type -> LT.Text
genType (TInt sign bits) =
  let base = if sign then "int" else "uint" :: LT.Text in
  [lt|#{base}#{show bits}_t|]
genType (TFloat False) =
  [lt|float|]
genType (TFloat True) =
  [lt|double|]
genType TBool =
  [lt|bool|]
genType TRaw =
  [lt|std::string|]
genType TString =
  [lt|std::string|]
genType (TList typ) =
  [lt|std::vector<#{genType typ} >|]
genType (TMap typ1 typ2) =
  [lt|std::map<#{genType typ1}, #{genType typ2} >|]
genType (TUserDef className params) =
  [lt|#{className}|]
genType (TTuple ts) =
  -- TODO: FIX
  foldr1 (\t1 t2 -> [lt|std::pair<#{t1}, #{t2} >|]) $ map genType ts
genType TObject =
  [lt|msgpack::object|]
genType TVoid =
  [lt|void|]
