{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java.Tuple (
  genTuple
  ) where

import qualified Data.Text.Lazy.IO as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.CodeGen.Java.Config
import Language.MessagePack.IDL.CodeGen.Java.Util

genTuple :: Config -> IO()
genTuple Config {..} = do
  LT.writeFile("Tuple.java") $ templ (configFilePath) [lt|
package #{configPackage};
public class Tuple<T, U> {
  public T a;
  public U b;
};
|]
