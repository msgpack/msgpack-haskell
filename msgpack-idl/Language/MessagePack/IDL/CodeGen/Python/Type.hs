{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Python.Type (
  fromMsgpack
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax
import Language.MessagePack.IDL.CodeGen.Python.Util

fromMsgpack :: Type -> T.Text -> LT.Text
fromMsgpack (TNullable t) name = fromMsgpack t name
fromMsgpack (TInt _ _) name = [lt|#{name}|]
fromMsgpack (TFloat False) name = [lt|#{name}|]
fromMsgpack (TFloat True) name = [lt|#{name}|]
fromMsgpack TBool name = [lt|#{name}|]
fromMsgpack TRaw name = [lt|#{name}|]
fromMsgpack TString name = [lt|#{name}|]
fromMsgpack (TList typ) name =
  let
    varname = T.append (T.pack "elem_") (T.map sanitize name) in
  [lt|[#{fromMsgpack typ varname} for #{varname} in #{name}]|]

fromMsgpack (TMap typ1 typ2) name =
  let
    keyname = T.append (T.pack "k_" ) $ T.map sanitize name
    valname = T.append (T.pack "v_" ) $ T.map sanitize name
  in
  [lt|{#{fromMsgpack typ1 keyname} : #{fromMsgpack typ2 valname} for #{keyname},#{valname} in #{name}.items()}|]

fromMsgpack (TUserDef className _) name = [lt|#{className}.from_msgpack(#{name})|]
            
fromMsgpack (TTuple ts) name =
            let elems = map (f name) (zip [0..] ts) in
            [lt| (#{LT.concat elems}) |]
            where
              f :: T.Text -> (Integer, Type) -> LT.Text
              f n (i, (TUserDef className _ )) = [lt|#{className}.from_msgpack(#{n}[#{show i}], |]
              f n (i, _) = [lt|#{n}[#{show i}], |]

fromMsgpack TObject name = [lt|#{name}|]
fromMsgpack TVoid _ = ""

