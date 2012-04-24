{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Ruby.ConvertingType (
  genConvertingType',
  genConvertingType
  ) where

import qualified Data.Text.Lazy as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax
import Language.MessagePack.IDL.CodeGen.Ruby.Util

genConvertingType :: LT.Text -> LT.Text -> Type -> LT.Text
genConvertingType unpacked v (TUserDef t _) = [lt|
#{v} = #{capitalizeT t}.new
#{v}.from_unpacked(#{unpacked})|]
genConvertingType _ _ _ = ""

genConvertingType' :: LT.Text -> LT.Text -> Type -> LT.Text
genConvertingType' unpacked v (TUserDef t p) = [lt|
#{genConvertingType unpacked v (TUserDef t p)}
return v|]
genConvertingType' unpacked _ _ = [lt|#{unpacked}|]

