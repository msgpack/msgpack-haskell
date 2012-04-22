{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java.Struct(
  genStruct
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax
import Language.MessagePack.IDL.CodeGen.Java.Resolve
import Language.MessagePack.IDL.CodeGen.Java.Util
import Language.MessagePack.IDL.CodeGen.Java.Decl
import Language.MessagePack.IDL.CodeGen.Java.Initialize

genStruct :: [(T.Text, Type)] -> FilePath -> Decl -> IO()
genStruct alias packageName MPMessage {..} = do
  let params = if null msgParam then "" else [lt|<#{T.intercalate ", " msgParam}>|]
      resolvedMsgFields = map (resolveFieldAlias alias) msgFields
      hashMapImport | not $ null [() | TMap _ _ <- map fldType resolvedMsgFields] = [lt|import java.util.HashMap;|]
                    | otherwise = ""
      arrayListImport | not $ null [() | TList _ <- map fldType resolvedMsgFields] = [lt|import java.util.ArrayList;|]
                      | otherwise = ""

  LT.writeFile ( (formatClassName $ T.unpack msgName) ++ ".java") [lt|
package #{packageName};

#{hashMapImport}
#{arrayListImport}

public class #{formatClassNameT msgName} #{params} {

#{LT.concat $ map genDecl resolvedMsgFields}
  public #{formatClassNameT msgName}() {
  #{LT.concat $ map genInit resolvedMsgFields}
  }
};
|]
genStruct _ _ _ = return ()
