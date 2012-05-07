{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java.Initialize (
  genInit
  ) where

import qualified Data.Text.Lazy as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

genInit :: Field -> LT.Text
genInit Field {..} = case fldDefault of
                      Nothing -> ""
                      Just defaultVal -> [lt| #{fldName} = #{genLiteral defaultVal};|]

genLiteral :: Literal -> LT.Text
genLiteral (LInt i) = [lt|#{show i}|]
genLiteral (LFloat d) = [lt|#{show d}|]
genLiteral (LBool b) = [lt|#{show b}|]
genLiteral LNull = [lt|null|]
genLiteral (LString s) = [lt|#{show s}|]

