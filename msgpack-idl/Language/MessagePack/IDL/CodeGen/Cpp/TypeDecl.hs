{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Cpp.TypeDecl ( 
  genTypeDeclMain
  ) where

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax
import Language.MessagePack.IDL.CodeGen.Cpp.Util
import Language.MessagePack.IDL.CodeGen.Cpp.Config
import Language.MessagePack.IDL.CodeGen.Cpp.NameSpace
import Language.MessagePack.IDL.CodeGen.Cpp.Type

genTypeDeclMain :: Config -> Spec -> IO()
genTypeDeclMain Config {..} spec = do
  let name = takeBaseName configFilePath
      once = map toUpper name
      ns = LT.splitOn "::" $ LT.pack configNameSpace
      typeHeader
        | configPFICommon =
          [lt|#include <msgpack.hpp>|]
        | otherwise =
          [lt|#include <msgpack.hpp>|]

  LT.writeFile (name ++ "_types.hpp") $ templ configFilePath once "TYPES" [lt|
#include <vector>
#include <map>
#include <string>
#include <stdexcept>
#include <stdint.h>
#{typeHeader}

#{genNameSpace ns $ LT.concat $ map (genTypeDecl name) spec }
|]

genTypeDecl :: String -> Decl -> LT.Text
genTypeDecl _ MPMessage {..} =
  genMsg msgName msgFields False
genTypeDecl _ MPException {..} =
  genMsg excName excFields True
genTypeDecl _ MPType { .. } =
  [lt|
typedef #{genType tyType} #{tyName};
|]
genTypeDecl _ _ = ""

genMsg name flds isExc =
  let fields = map f flds
      fs = map (maybe undefined fldName) $ sortField flds
  in [lt|
struct #{name}#{e} {
public:

  #{destructor}
  MSGPACK_DEFINE(#{T.intercalate ", " fs});  
#{LT.concat fields}
};
|]
  where
    e = if isExc then [lt| : public std::exception|] else ""
    destructor = if isExc then [lt|~#{name}() throw() {}
|] else ""
    f Field {..} = [lt|
  #{genType fldType} #{fldName};|]
