{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Ruby.AttrWriter (
  genAttrWriter
  ) where

import Data.Char
import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

import Language.MessagePack.IDL.CodeGen.Ruby.ConvertingType
import Language.MessagePack.IDL.CodeGen.Ruby.Util

-- TODO: Check when val is not null with TNullable
-- TODO: Write single precision value on TFloat False
genAttrWriter :: Field -> LT.Text
genAttrWriter Field {..} = genAttrWriter' fldType fldName

genAttrWriter' :: Type -> T.Text -> LT.Text

genAttrWriter' TBool n = [lt|
def #{n}=(val)
  @#{n} = val.to_b
end
|]

genAttrWriter' (TMap kt vt) n = [lt|
def #{n}=(val)
  @#{n} = {}
  val.each do |k, v|
#{indent 4 $ convert "k" "newk" kt}
#{indent 4 $ convert "v" "newv" vt}
  end
end
|]
  where
    convert from to (TUserDef t p) =
        genConvertingType from to (TUserDef t p)
    convert from to _ = [lt|#{to} = #{from}|]

genAttrWriter' (TUserDef name types) n = [lt|
def #{n}=(val)
#{indent 2 $ convert "val" atn (TUserDef name types)}
end
|]
  where
    atn = [lt|@#{n}|]
    convert from to (TUserDef t p) =
        genConvertingType from to (TUserDef t p)

genAttrWriter' _ _ = ""

