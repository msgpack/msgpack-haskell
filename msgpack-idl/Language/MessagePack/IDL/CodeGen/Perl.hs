{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Perl (
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
    }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} spec = do
  let name = takeBaseName configFilePath
      once = map toUpper name
      ns = LT.splitOn "::" $ LT.pack configNameSpace

  LT.writeFile (name ++ "_types.pm") $ templ configFilePath once "TYPES" [lt|
package types;
use strict;
use warnings;

#{genNameSpace ns $ LT.concat $ map (genTypeDecl name) spec }
|]

  LT.writeFile (name ++ "_server.pm") $ templ configFilePath once "SERVER" [lt|
package server;
use strict;
use warnings;

#{genNameSpace (snoc ns "server") $ LT.concat $ map genServer spec}
|]

  LT.writeFile (name ++ "_client.pm") [lt|
package client;
use strict;
use warnings;

#{genNameSpace (snoc ns "client") $ LT.concat $ map genClient spec}
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
struct #{name} {
#{LT.concat fields}
};
|]
  where
    f Field {..} = [lt|
  #{genType fldType} #{fldName};|]

sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix ->
  find ((==ix). fldId) flds

genServer :: Decl -> LT.Text
genServer MPService {..} = [lt|
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

genServer _ = ""

genClient :: Decl -> LT.Text
genClient MPService {..} = [lt|
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

genClient _ = ""

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
genType TVoid =
  [lt|void|]

templ :: FilePath -> String -> String -> LT.Text -> LT.Text
templ filepath once name content = [lt|
// This file is auto-generated from #{filepath}
// *** DO NOT EDIT ***

#{content}

|]

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
