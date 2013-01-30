{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Cpp (
  Config(..),
  generate,
  ) where

import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath
    , configNameSpace :: String
    , configPFICommon :: Bool
    }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} spec = do
  let name = takeBaseName configFilePath
      once = map toUpper name
      ns = LT.splitOn "::" $ LT.pack configNameSpace

      typeHeader
        | configPFICommon =
          [lt|#include <msgpack.hpp>|]
        | otherwise =
          [lt|#include <msgpack.hpp>|]
      serverHeader
        | configPFICommon =
          [lt|#include <pficommon/network/mprpc.h>
#include <pficommon/lang/bind.h>|]
        | otherwise =
          [lt|#include <msgpack/rpc/server.h>|]
      clientHeader
        | configPFICommon =
          [lt|#include <pficommon/network/mprpc.h>|]
        | otherwise =
          [lt|#include <msgpack/rpc/client.h>|]
  
  LT.writeFile (name ++ "_types.hpp") $ templ configFilePath ns once "TYPES" [lt|
#include <vector>
#include <map>
#include <string>
#include <stdexcept>
#include <stdint.h>
#{typeHeader}

#{genNameSpace ns $ LT.concat $ map (genTypeDecl name) spec }
|]

  LT.writeFile (name ++ "_server.hpp") $ templ configFilePath (snoc ns "server") once "SERVER" [lt|
#include "#{name}_types.hpp"
#{serverHeader}

#{genNameSpace (snoc ns "server") $ LT.concat $ map (genServer configPFICommon) spec}
|]

  LT.writeFile (name ++ "_client.hpp") $ templ configFilePath (snoc ns "client") once "CLIENT" [lt|
#include "#{name}_types.hpp"
#{clientHeader}

#{genNameSpace (snoc ns "client") $ LT.concat $ map (genClient configPFICommon) spec}
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

sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix ->
  find ((==ix). fldId) flds

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
    let typs = map (genRetType . maybe Nothing (Just . fldType)) $ sortField methodArgs in
    let params = map g methodArgs in
    case params of
      [] -> [lt|
      if (method == "#{methodName}") {
        req.result<#{genRetType methodRetType} >(static_cast<Impl*>(this)->#{methodName}());
        return;
      }
|]
      _ -> [lt|
      if (method == "#{methodName}") {
        msgpack::type::tuple<#{LT.intercalate ", " typs} > params;
        req.params().convert(&params);
        req.result<#{genRetType methodRetType} >(static_cast<Impl*>(this)->#{methodName}(#{LT.intercalate ", " params}));
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
      let typs = map (genRetType . maybe Nothing (Just . fldType)) $ sortField methodArgs
          sign = [lt|#{genRetType methodRetType}(#{LT.intercalate ", " typs})|]
          phs  = LT.concat $ [[lt|, pfi::lang::_#{show ix}|] | ix <- [1 .. length (typs)]]
      in [lt|
    rpc_server::add<#{sign} >("#{methodName}", pfi::lang::bind(&Impl::#{methodName}, static_cast<Impl*>(this)#{phs}));|]

    genSetMethod _ = ""

genServer _ _ = ""

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
      Nothing -> [lt|
    void #{methodName}(#{args}) {
      c_.call("#{methodName}"#{vals});
    }
|]
      Just typ -> [lt|
    #{genType typ} #{methodName}(#{args}) {
      return c_.call("#{methodName}"#{vals}).get<#{genType typ} >();
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
    let typs = map (genRetType . maybe Nothing (\f -> Just (fldType f))) $ sortField methodArgs
        sign = [lt|#{genRetType methodRetType}(#{LT.intercalate ", " typs})|]
        args = LT.intercalate ", " $ map arg methodArgs
        vals = LT.intercalate ", " $ map val methodArgs in
    case methodRetType of
      Nothing -> [lt|
    void #{methodName}(#{args}) {
      call<#{sign}>("#{methodName}")(#{vals});
    }
|]
      Just t -> [lt|
    #{genType t} #{methodName}(#{args}) {
      return call<#{sign}>("#{methodName}")(#{vals});
    }
|]
    where
      arg Field {..} = [lt|#{genType fldType} #{fldName}|]
      val Field {..} = [lt|#{fldName}|]

  genMethodCall _ = ""

genClient _ _ = ""

genType :: Type -> LT.Text
genType (TInt sign bits) =
  let base = if sign then "int" else "uint" :: LT.Text in
  [lt|#{base}#{show bits}_t|]
genType (TFloat False) =
  [lt|float|]
genType (TFloat True) =
  [lt|double|]
genType TBool =
  [lt|bool|]
genType TRaw =
  [lt|std::string|]
genType TString =
  [lt|std::string|]
genType (TList typ) =
  [lt|std::vector<#{genType typ} >|]
genType (TMap typ1 typ2) =
  [lt|std::map<#{genType typ1}, #{genType typ2} >|]
genType (TUserDef className params) =
  [lt|#{className}|]
genType (TTuple ts) =
  -- TODO: FIX
  foldr1 (\t1 t2 -> [lt|std::pair<#{t1}, #{t2} >|]) $ map genType ts
genType TObject =
  [lt|msgpack::object|]

genRetType :: Maybe Type -> LT.Text
genRetType Nothing = [lt|void|]
genRetType (Just t) = genType t

templ :: FilePath -> [LT.Text] -> String -> String -> LT.Text -> LT.Text
templ filepath ns once name content = [lt|
// This file is auto-generated from #{filepath}
// *** DO NOT EDIT ***

#ifndef #{namespace}_#{once}_#{name}_HPP_
#define #{namespace}_#{once}_#{name}_HPP_

#{content}

#endif // #{namespace}_#{once}_#{name}_HPP_
|] where
   namespace = LT.intercalate "_" $ map LT.toUpper ns


genNameSpace :: [LT.Text] -> LT.Text -> LT.Text
genNameSpace namespace content = f namespace
  where
    f [] = [lt|#{content}|]
    f (n:ns) = [lt|
namespace #{n} {
#{f ns}
} // namespace #{n}
|]

snoc xs x = xs ++ [x]
