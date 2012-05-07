{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Python.Util where

import Data.List
import qualified Data.Text.Lazy as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|
# This file is auto-generated from #{filepath}
# *** DO NOT EDIT ***

#{content}
|]

sanitize :: Char -> Char
sanitize '[' = '_'
sanitize ']' = '_'
sanitize c = c

sortField :: [Field] -> [Maybe Field]
sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix ->
  find ((==ix). fldId) flds

