{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java.Exception (
  genException
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

import Language.MessagePack.IDL.CodeGen.Java.Util
import Language.MessagePack.IDL.CodeGen.Java.Initialize
import Language.MessagePack.IDL.CodeGen.Java.Decl

genException :: FilePath -> Decl -> IO()
genException packageName MPException {..} = do
  LT.writeFile ( (formatClassName $ T.unpack excName) ++ ".java") [lt|
package #{packageName};

public class #{formatClassNameT excName} #{params}{

#{LT.concat $ map genDecl excFields}
  public #{formatClassNameT excName}() {
  #{LT.concat $ map genInit excFields}
  }
};
|]
  where
    params = if null excParam then "" else [lt|<#{T.intercalate ", " excParam}>|]
    super = case excSuper of 
              Just x -> [st|extends #{x}|]
              Nothing -> ""
genException _ _ = return ()
