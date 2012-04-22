{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Cpp.Server (
  genServerMain
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

genServerMain :: Config -> Spec -> IO ()
genServerMain Config {..} spec = do
  let name = takeBaseName configFilePath
      once = map toUpper name
      ns = LT.splitOn "::" $ LT.pack configNameSpace
      serverHeader
        | configPFICommon =
          [lt|#include <pficommon/network/mprpc.h>
#include <pficommon/lang/bind.h>|]
        | otherwise =
          [lt|#include <msgpack/rpc/server.h>|]

  LT.writeFile (name ++ "_server.hpp") $ templ configFilePath once "SERVER" [lt|
#include "#{name}_types.hpp"
#{serverHeader}

#{genNameSpace (snoc ns "server") $ LT.concat $ map (genServer configPFICommon) spec}
|]

genServer :: Bool -> Decl -> LT.Text
genServer False MPService {..} = [lt|
template <class Impl>
class #{serviceName} : public msgpack::rpc::server::base {
public:

  void dispatch(msgpack::rpc::request req) {
    try {
      std::string method;
      req.method().convert(&method);
#{LT.concat $ map genMethodDispatch serviceMethods}
    } catch (const msgpack::type_error& e) {
      req.error(msgpack::rpc::ARGUMENT_ERROR);
    } catch (const std::exception& e) {
      req.error(std::string(e.what()));
    }
  }
};
|]
  where
  genMethodDispatch Function {..} =
    -- TODO: FIX IT!
    let typs = map (genType . maybe TVoid fldType) $ sortField methodArgs in
    let params = map g methodArgs in
    case params of
      [] -> [lt|
      if (method == "#{methodName}") {
        req.result<#{genType methodRetType} >(static_cast<Impl*>(this)->#{methodName}());
        return;
      }
|]
      _ -> [lt|
      if (method == "#{methodName}") {
        msgpack::type::tuple<#{LT.intercalate ", " typs} > params;
        req.params().convert(&params);
        req.result<#{genType methodRetType} >(static_cast<Impl*>(this)->#{methodName}(#{LT.intercalate ", " params}));
        return;
      }
|]
    where
    g fld = [lt|params.get<#{show $ fldId fld}>()|]

  genMethodDispatch _ = ""

genServer True MPService {..} = [lt|
template <class Impl>
class #{serviceName} : public pfi::network::mprpc::rpc_server {
public:
  #{serviceName}(double timeout_sec): rpc_server(timeout_sec) {
#{LT.concat $ map genSetMethod serviceMethods}
  }
};
|]
  where
    genSetMethod Function {..} =
      let typs = map (genType . maybe TVoid fldType) $ sortField methodArgs
          sign = [lt|#{genType methodRetType}(#{LT.intercalate ", " typs})|]
          phs  = LT.concat $ [[lt|, pfi::lang::_#{show ix}|] | ix <- [1 .. length (typs)]]
      in [lt|
    rpc_server::add<#{sign} >("#{methodName}", pfi::lang::bind(&Impl::#{methodName}, static_cast<Impl*>(this)#{phs}));|]

    genSetMethod _ = ""

genServer _ _ = ""
