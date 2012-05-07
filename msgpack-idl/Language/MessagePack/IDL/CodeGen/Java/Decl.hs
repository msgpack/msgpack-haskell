{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java.Decl (
  genDecl,
  genType
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax
import Language.MessagePack.IDL.CodeGen.Java.Util
import Language.MessagePack.IDL.CodeGen.Java.Alias

genDecl :: Field -> LT.Text
genDecl Field {..} = 
    [lt|  public #{genType fldType} #{fldName};
|]

genType :: Type -> LT.Text
genType (TInt _ bits) = case bits of
                            8 -> [lt|byte|]
                            16 -> [lt|short|]
                            32 -> [lt|int|]
                            64 -> [lt|long|]
                            _ -> [lt|int|]
genType (TFloat False) =
  [lt|float|]
genType (TFloat True) =
  [lt|double|]
genType TBool =
  [lt|boolean|]
genType TRaw =
  [lt|String|]
genType TString =
  [lt|String|]
genType (TList typ) =
  [lt|ArrayList<#{genWrapperType typ} >|]
genType (TMap typ1 typ2) =
  [lt|HashMap<#{genType typ1}, #{genType typ2} >|]
genType (TUserDef className params) =
  [lt|#{formatClassNameT className} #{associateBracket $ map genType params}|]
genType (TTuple ts) =
  -- TODO: FIX
  foldr1 (\t1 t2 -> [lt|Tuple<#{t1}, #{t2} >|]) $ map genWrapperType ts
genType TObject =
  [lt|org.msgpack.type.Value|]
genType TVoid =
  [lt|void|]

genTypeWithContext :: Spec -> Type -> LT.Text
genTypeWithContext spec t = case t of 
                              (TUserDef className params) -> 
                                  case lookup className $ map genAlias $ filter isMPType spec of
                                    Just x -> genType x
                                    Nothing -> ""
                              otherwise -> genType t

genTypeWithTypedef :: T.Text -> Decl -> Maybe Type
genTypeWithTypedef className MPType {..} =
  if className == tyName then Just tyType else Nothing
genTypeWithTypedef className _ = Nothing

genWrapperType :: Type -> LT.Text
genWrapperType (TInt _ bits) = case bits of
                                 8 -> [lt|Byte|]
                                 16 -> [lt|Short|]
                                 32 -> [lt|Integer|]
                                 64 -> [lt|Long|]
                                 _ -> [lt|Integer|]
genWrapperType (TFloat False) =
  [lt|Float|]
genWrapperType (TFloat True) =
  [lt|Double|]
genWrapperType TBool =
  [lt|Boolean|]
genWrapperType TRaw =
  [lt|String|]
genWrapperType TString =
  [lt|String|]
genWrapperType (TList typ) =
  [lt|ArrayList<#{genWrapperType typ} >|]
genWrapperType (TMap typ1 typ2) =
  [lt|HashMap<#{genWrapperType typ1}, #{genWrapperType typ2} >|]
genWrapperType (TUserDef className params) =
  [lt|#{formatClassNameT className} #{associateBracket $ map genWrapperType params}|]
genWrapperType (TTuple ts) =
  -- TODO: FIX
  foldr1 (\t1 t2 -> [lt|Tuple<#{t1}, #{t2} >|]) $ map genWrapperType ts
genWrapperType TObject =
  [lt|org.msgpack.type.Value|]
genWrapperType TVoid =
  [lt|void|]

