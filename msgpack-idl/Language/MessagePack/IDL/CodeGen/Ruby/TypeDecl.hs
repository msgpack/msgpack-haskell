{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Ruby.TypeDecl (
  genTypeDeclMain
  ) where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax
import Language.MessagePack.IDL.CodeGen.Ruby.Util
import Language.MessagePack.IDL.CodeGen.Ruby.Config
import Language.MessagePack.IDL.CodeGen.Ruby.Module
import Language.MessagePack.IDL.CodeGen.Ruby.AttrWriter
import Language.MessagePack.IDL.CodeGen.Ruby.Accessor

genTypeDeclMain name once mods Config {..} spec = LT.writeFile (name ++ "_types.rb") $ templ configFilePath [lt|
require 'msgpack/rpc'

#{genModule mods $ LT.concat $ map (genTypeDecl name) spec }|]
  

genTypeDecl :: String -> Decl -> LT.Text
genTypeDecl _ MPMessage {..} =
  genMsg msgName msgFields False

genTypeDecl _ MPException {..} =
  genMsg excName excFields True
  
genTypeDecl _ _ = ""

genMsg name flds isExc = [lt|
class #{capitalizeT name}#{deriveError}
  def to_msgpack(out = '')
    [#{afs}].to_msgpack(out)
  end

  def from_unpacked(unpacked)
    #{afs} = unpacked
  end

#{indent 2 $ LT.concat writers}

#{indent 2 $ genAccessors sorted_flds}
end
|]
  where
    sorted_flds = sortField flds
    fs = map (maybe undefined fldName) sorted_flds
    writers = map (maybe undefined genAttrWriter) sorted_flds
    afs = T.intercalate ", " $ map (mappend "@") fs
    deriveError = if isExc then [lt| < StandardError|] else ""

