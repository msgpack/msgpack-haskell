{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Cpp.Client (
  genClientMain
  ) where

import Data.Char
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax
import Language.MessagePack.IDL.CodeGen.Cpp.Util
import Language.MessagePack.IDL.CodeGen.Cpp.Config
import Language.MessagePack.IDL.CodeGen.Cpp.NameSpace
import Language.MessagePack.IDL.CodeGen.Cpp.Type

genClientMain :: Config -> Spec -> IO()
genClientMain Config {..} spec = do
  let name = takeBaseName configFilePath
      once = map toUpper name
      ns = LT.splitOn "::" $ LT.pack configNameSpace
      clientHeader
        | configPFICommon =
          [lt|#include <pficommon/network/mprpc.h>|]
        | otherwise =
          [lt|#include <msgpack/rpc/client.h>|]

  LT.writeFile (name ++ "_client.hpp") [lt|
#include "#{name}_types.hpp"
#{clientHeader}

#{genNameSpace (snoc ns "client") $ LT.concat $ map (genClient configPFICommon) spec}
|]

genClient :: Bool -> Decl -> LT.Text
genClient False MPService {..} = [lt|
class #{serviceName} {
public:
  #{serviceName}(const std::string &host, uint64_t port)
    : c_(host, port) {}
#{LT.concat $ map genMethodCall serviceMethods}
private:
  msgpack::rpc::client c_;
};
|]
  where
  genMethodCall Function {..} =
    let args = LT.intercalate ", " $ map arg methodArgs in
    let vals = LT.concat $ map val methodArgs in
    case methodRetType of
      TVoid -> [lt|
    void #{methodName}(#{args}) {
      c_.call("#{methodName}"#{vals});
    }
|]
      _ -> [lt|
    #{genType methodRetType} #{methodName}(#{args}) {
      return c_.call("#{methodName}"#{vals}).get<#{genType methodRetType} >();
    }
|]
    where
      arg Field {..} = [lt|#{genType fldType} #{fldName}|]
      val Field {..} = [lt|, #{fldName}|]

  genMethodCall _ = ""

genClient True MPService {..} = [lt|
class #{serviceName} : public pfi::network::mprpc::rpc_client {
public:
  #{serviceName}(const std::string &host, uint64_t port, double timeout_sec)
    : rpc_client(host, port, timeout_sec) {}
#{LT.concat $ map genMethodCall serviceMethods}
private:
};
|]
  where
  genMethodCall Function {..} =
    let typs = map (genType . maybe TVoid fldType) $ sortField methodArgs
        sign = [lt|#{genType methodRetType}(#{LT.intercalate ", " typs})|]
        args = LT.intercalate ", " $ map arg methodArgs
        vals = LT.intercalate ", " $ map val methodArgs in
    case methodRetType of
      TVoid -> [lt|
    void #{methodName}(#{args}) {
      call<#{sign}>("#{methodName}")(#{vals});
    }
|]
      _ -> [lt|
    #{genType methodRetType} #{methodName}(#{args}) {
      return call<#{sign}>("#{methodName}")(#{vals});
    }
|]
    where
      arg Field {..} = [lt|#{genType fldType} #{fldName}|]
      val Field {..} = [lt|#{fldName}|]

  genMethodCall _ = ""

genClient _ _ = ""
