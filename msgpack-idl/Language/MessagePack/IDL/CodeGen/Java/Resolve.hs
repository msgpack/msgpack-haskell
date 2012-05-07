{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java.Resolve where

import qualified Data.Text as T

import Language.MessagePack.IDL.Syntax

resolveMethodAlias :: [(T.Text, Type)] -> Method -> Method
resolveMethodAlias alias Function {..}  = Function methodInherit methodName (resolveTypeAlias alias methodRetType) (map (resolveFieldAlias alias) methodArgs)
resolveMethodAlias _ f = f

resolveFieldAlias :: [(T.Text, Type)] -> Field -> Field
resolveFieldAlias alias Field {..} = Field fldId (resolveTypeAlias alias fldType) fldName fldDefault

resolveTypeAlias :: [(T.Text, Type)] -> Type -> Type
resolveTypeAlias alias ty = let fixedAlias = resolveTypeAlias alias in 
                           case ty of
                             TNullable t ->
                                 TNullable $ fixedAlias t
                             TList t ->
                                 TList $ fixedAlias t
                             TMap s t ->
                                 TMap (fixedAlias s) (fixedAlias t)
                             TTuple ts ->
                                 TTuple $ map fixedAlias ts
                             TUserDef className params ->
                                 case lookup className alias of 
                                   Just resolvedType -> resolvedType
                                   Nothing -> TUserDef className (map fixedAlias params)
                             otherwise -> ty

