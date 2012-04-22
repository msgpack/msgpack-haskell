{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java.Client (
  genClient
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

import Language.MessagePack.IDL.CodeGen.Java.Config
import Language.MessagePack.IDL.CodeGen.Java.Resolve
import Language.MessagePack.IDL.CodeGen.Java.Util
import Language.MessagePack.IDL.CodeGen.Java.Method

genClient :: [(T.Text, Type)] -> Config -> Decl -> IO()
genClient alias Config {..} MPService {..} = do 
  let resolvedServiceMethods = map (resolveMethodAlias alias) serviceMethods
      hashMapImport | not $ null [() | TMap _ _ <- map methodRetType resolvedServiceMethods ] = [lt|import java.util.HashMap;|]
                    | otherwise = ""
      arrayListImport | not $ null [() | TList _ <- map methodRetType resolvedServiceMethods] = [lt|import java.util.ArrayList;|]
                      | otherwise = ""

  LT.writeFile (T.unpack className ++ ".java") $ templ configFilePath [lt|
package #{configPackage};

#{hashMapImport}
#{arrayListImport}
import org.msgpack.rpc.Client;
import org.msgpack.rpc.loop.EventLoop;

public class #{className} {
  public #{className}(String host, int port, double timeout_sec) throws Exception {
    EventLoop loop = EventLoop.defaultEventLoop();
    c_ = new Client(host, port, loop);
    iface_ = c_.proxy(RPCInterface.class);
  }

  public static interface RPCInterface {
#{LT.concat $ map genSignature resolvedServiceMethods}
  }

#{LT.concat $ map genMethodCall resolvedServiceMethods}
  private Client c_;
  private RPCInterface iface_;
};
|]
  where
    className = (formatClassNameT serviceName) `T.append` "Client"

genClient _ _ _ = return ()

