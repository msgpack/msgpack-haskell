{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Cpp.Util where

import Data.List
import qualified Data.Text.Lazy as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix ->
  find ((==ix). fldId) flds

snoc xs x = xs ++ [x]

templ :: FilePath -> String -> String -> LT.Text -> LT.Text
templ filepath once name content =[lt|
// This file is auto-generated from #{filepath}
// *** DO NOT EDIT ***

#ifndef #{once}_#{name}_HPP_
#define #{once}_#{name}_HPP_

#{content}

#endif // #{once}_#{name}_HPP_
|]


