{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java.Alias where

import qualified Data.Text as T

import Language.MessagePack.IDL.Syntax

genAlias :: Decl -> (T.Text, Type)
genAlias MPType {..} = (tyName, tyType)
genAlias _ = ("", TBool)

isMPType :: Decl -> Bool
isMPType MPType {..} = True
isMPType _ = False

