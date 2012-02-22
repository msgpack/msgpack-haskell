{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Erlang (
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
    }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} spec = do
  let name = takeBaseName configFilePath
      once = map toUpper name

      headerFile = name ++ "_types.hrl"
  
  LT.writeFile (headerFile) $ templ configFilePath once "TYPES" [lt|
-ifndef(#{once}).
-define(#{once}, 1).

#{LT.concat $ map (genTypeDecl name) spec }

-endif.
|]

  LT.writeFile (name ++ "_server.tmpl.erl") $ templ configFilePath once "SERVER" [lt|

-module(#{name}_server).
-author(@msgpack-idl).

-include("#{headerFile}").
-behaviour(gen_msgpack_rpc_srv).

-record(state, {}).
#{LT.concat $ map genServer spec}
|]

  LT.writeFile (name ++ "_client.tmpl.erl") [lt|

-module(#{name}_client).
-author(@msgpack-idl).

-include("#{headerFile}").
#{LT.concat $ map genClient spec}
|]

genTypeDecl :: String -> Decl -> LT.Text
genTypeDecl _ MPMessage {..} =
  genMsg msgName msgFields False

genTypeDecl _ MPException {..} =
  genMsg excName excFields True
  
genTypeDecl _ MPType { .. } =
  [lt|
-type #{tyName}() :: #{genType tyType}.
|]

genTypeDecl _ _ = ""

genMsg name flds isExc =
  let fields = map f flds
  in [lt|
-type #{name}() :: [
      #{LT.intercalate "\n    | " fields}
    ]. % #{e}
|]
  where
    e = if isExc then [lt| : public std::exception|] else ""
    f Field {..} = [lt|#{genType fldType} %  #{fldName}|]

sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix ->
  find ((==ix). fldId) flds

makeExport Function {..} = [lt|#{methodName}/#{show $ length methodArgs}|]
makeExport _ = ""


genServer :: Decl -> LT.Text
genServer MPService {..} = [lt|

-export([#{LT.intercalate ", " $ map makeExport serviceMethods}]).

init(_Argv)-> {ok, #state{}}.
                            
% TODO enable #{serviceName}
#{LT.concat $ map genSetMethod serviceMethods}

|]
  where
    genSetMethod Function {..} =
      let typs = map (genType . maybe TVoid fldType) $ sortField methodArgs
          args = map f methodArgs
          f Field {..} = [lt|#{capitalize0 fldName}|]
          capitalize0 str = T.cons (toUpper $ T.head str) (T.tail str)

      in [lt|
-spec #{methodName}(#{LT.intercalate ", " typs}) -> #{genType methodRetType}.
#{methodName}(#{LT.intercalate ", " args}) ->
  Reply = ok,  % write your code here
  {reply, Reply}.
|]
--    rpc_server::add<#{sign} >("#{methodName}", pfi::lang::bind(&Impl::#{methodName}, static_cast<Impl*>(this)#{phs}));
    genSetMethod _ = ""

genServer _ = ""

genClient :: Decl -> LT.Text
genClient MPService {..} = [lt|

% TODO: enable services  #{serviceName}

-export([#{LT.intercalate ", " $ map makeExport serviceMethods}]).  
  
%  #{serviceName}(const std::string &host, uint64_t port, double timeout_sec)
%

#{LT.concat $ map genMethodCall serviceMethods}
|]
  where
  genMethodCall Function {..} =
      let typs = map (genType . maybe TVoid fldType) $ sortField methodArgs
          args = map f methodArgs
          f Field {..} = [lt|#{capitalize0 fldName}|]
          capitalize0 str = T.cons (toUpper $ T.head str) (T.tail str)
      in [lt|
-spec #{methodName}(#{LT.intercalate ", " typs}) -> #{genType methodRetType}.
#{methodName}(#{LT.intercalate ", " args}) ->
   mprc::call(...).
|]
    where
      arg Field {..} = [lt|#{genType fldType} #{fldName}|]
      val Field {..} = [lt|#{fldName}|]

  genMethodCall _ = ""

genClient _ = ""

genType :: Type -> LT.Text
genType (TInt sign bits) =
  let base = if sign then "non_neg_integer" else "integer" :: LT.Text in
  [lt|#{base}()|]
genType (TFloat False) =
  [lt|float()|]
genType (TFloat True) =
  [lt|double()|]
genType TBool =
  [lt|boolean()|]
genType TRaw =
  [lt|binary()|]
genType TString =
  [lt|string()|]
genType (TList typ) =
  [lt|list(#{genType typ})|]
genType (TMap typ1 typ2) =
  [lt|list({#{genType typ1}, #{genType typ2}})|]
genType (TUserDef className params) =
  [lt|#{className}()|]
genType (TTuple ts) =
  -- TODO: FIX
  foldr1 (\t1 t2 -> [lt|std::pair<#{t1}, #{t2} >|]) $ map genType ts
genType TObject =
  [lt|term()|]
genType TVoid =
  [lt|void()|]

templ :: FilePath -> String -> String -> LT.Text -> LT.Text
templ filepath once name content = [lt|
% This file is auto-generated from #{filepath}
% *** DO NOT EDIT ***

#{content}

|]
