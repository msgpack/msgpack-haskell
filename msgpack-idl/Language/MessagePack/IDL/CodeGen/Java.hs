{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Java (
  Config(..),
  generate,
  ) where

import Data.Char
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import System.Directory
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath
    , configPackage :: String
    }
  deriving (Show, Eq)

generate :: Config -> Spec -> IO()
generate config spec = do
  let typeAlias = map genAlias $ filter isMPType spec
      dirName = joinPath $ map LT.unpack $ LT.split (== '.') $ LT.pack $ configPackage config

  genTuple config
  createDirectoryIfMissing True dirName
  mapM_ (genClient typeAlias config) spec
  mapM_ (genStruct typeAlias config) spec
  mapM_ (genException typeAlias config) spec

{--
  LT.writeFile (name ++ "Server.java") $ templ (configFilePath ++ configPackage ++"/server/")[lt|
import org.msgpack.rpc.Server;
package #{configPackage}

#{LT.concat $ map genServer spec}
|]
--}

genTuple :: Config -> IO()
genTuple Config {..} = do
  LT.writeFile("Tuple.java") $ templ (configFilePath) [lt|
package #{configPackage};
public class Tuple<T, U> {
  public T a;
  public U b;
};
|]

genImport :: FilePath -> Decl -> LT.Text
genImport packageName MPMessage {..} = 
    [lt|import #{packageName}.#{formatClassNameT msgName};
|]
genImport _ _ = ""

genStruct :: [(T.Text, Type)] -> Config -> Decl -> IO()
genStruct alias Config{..} MPMessage {..} = do
  let params = if null msgParam then "" else [lt|<#{T.intercalate ", " msgParam}>|]
      resolvedMsgFields = map (resolveFieldAlias alias) msgFields
      hashMapImport | not $ null [() | TMap _ _ <- map fldType resolvedMsgFields] = [lt|import java.util.HashMap;|]
                    | otherwise = ""
      arrayListImport | not $ null [() | TList _ <- map fldType resolvedMsgFields] = [lt|import java.util.ArrayList;|]
                      | otherwise = ""
      dirName = joinPath $ map LT.unpack $ LT.split (== '.') $ LT.pack configPackage
      fileName =  dirName ++ "/" ++ (T.unpack $ formatClassNameT msgName) ++ ".java"

  LT.writeFile fileName $ templ configFilePath [lt|
package #{configPackage};

#{hashMapImport}
#{arrayListImport}
import org.msgpack.MessagePack;
import org.msgpack.annotation.Message;

@Message
public class #{formatClassNameT msgName} #{params} {

#{LT.concat $ map genDecl resolvedMsgFields}
  public #{formatClassNameT msgName}() {
  #{LT.concat $ map genInit resolvedMsgFields}
  }
};
|]
genStruct _ _ _ = return ()

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

genInit :: Field -> LT.Text
genInit Field {..} = case fldDefault of
                      Nothing -> ""
                      Just defaultVal -> [lt| #{fldName} = #{genLiteral defaultVal};|]

genDecl :: Field -> LT.Text
genDecl Field {..} = 
    [lt|  public #{genType fldType} #{fldName};
|]

genException :: [(T.Text, Type)] -> Config -> Decl -> IO()
genException alias Config{..} MPException{..} = do
  LT.writeFile ( (formatClassName $ T.unpack excName) ++ ".java") $ templ configFilePath [lt|
package #{configPackage};

import org.msgpack.MessagePack;
import org.msgpack.annotation.Message;

@Message
public class #{formatClassNameT excName} #{params}{
#{LT.concat $ map genDecl excFields}
  public #{formatClassNameT excName}() {
  #{LT.concat $ map genInit excFields}
  }
};
|]
  where
    params = if null excParam then "" else [lt|<#{T.intercalate ", " excParam}>|]
    super = case excSuper of 
              Just x -> [st|extends #{x}|]
              Nothing -> ""
genException _ _ _  = return ()

genClient :: [(T.Text, Type)] -> Config -> Decl -> IO()
genClient alias Config {..} MPService {..} = do 
  let resolvedServiceMethods = map (resolveMethodAlias alias) serviceMethods
      hashMapImport | not $ null [() | TMap _ _ <- map methodRetType resolvedServiceMethods ] = [lt|import java.util.HashMap;|]
                    | otherwise = ""
      arrayListImport | not $ null [() | TList _ <- map methodRetType resolvedServiceMethods] = [lt|import java.util.ArrayList;|]
                      | otherwise = ""
      dirName = joinPath $ map LT.unpack $ LT.split (== '.') $ LT.pack configPackage
      fileName =  dirName ++ "/" ++ (T.unpack className) ++ ".java"

  LT.writeFile fileName $ templ configFilePath [lt|
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
    className = (formatClassNameT serviceName) `mappend` "Client"
    genMethodCall Function {..} =
        let args = T.intercalate ", " $ map genArgs' methodArgs
            vals = T.intercalate ", " $ pack methodArgs genVal in
        case methodRetType of
          TVoid -> [lt|
  public void #{methodName}(#{args}) {
    iface_.#{methodName}(#{vals});
  }
|]
          _ -> [lt|
  public #{genType methodRetType} #{methodName}(#{args}) {
    return iface_.#{methodName}(#{vals});
  }
|]
    genMethodCall _ = ""

genClient _ _ _ = return ()

genSignature :: Method -> LT.Text
genSignature Function {..} = 
    [lt|    #{genType methodRetType} #{methodName}(#{args});
|]
    where
      args = (T.intercalate ", " $ map genArgs' methodArgs)
genSignature  _ = ""

genArgs :: Maybe Field -> T.Text
genArgs (Just field) = genArgs' field
genArgs Nothing = ""

genArgs' :: Field -> T.Text
genArgs' Field {..} = [st|#{genType fldType} #{fldName}|]

pack :: [Field] -> (Maybe Field -> T.Text) -> [T.Text]
pack fields converter=
  let ixs = map (\f -> fldId f) fields
      dic = zip ixs [0..]
      m = maximum (-1 :ixs)
      sortedIxs = [ lookup ix dic | ix <- [0..m]] :: [Maybe Int] in
  map (\sIx -> case sIx of 
                 Nothing -> converter Nothing 
                 Just i  -> converter $ Just (fields!!i) ) sortedIxs

genVal :: Maybe Field -> T.Text
genVal Nothing = "null"
genVal (Just field) = fldName field

formatClassNameT :: T.Text -> T.Text
formatClassNameT = T.pack . formatClassName . T.unpack

formatClassName :: String -> String
formatClassName = concatMap (\(c:cs) -> toUpper c:cs) . words . map (\c -> if c=='_' then ' ' else c)

genServer :: Decl -> LT.Text
genServer _ = ""

genLiteral :: Literal -> LT.Text
genLiteral (LInt i) = [lt|#{show i}|]
genLiteral (LFloat d) = [lt|#{show d}|]
genLiteral (LBool b) = [lt|#{show b}|]
genLiteral LNull = [lt|null|]
genLiteral (LString s) = [lt|#{show s}|]

associateBracket :: [LT.Text] -> LT.Text
associateBracket msgParam = 
  if null msgParam then "" else [lt|<#{LT.intercalate ", " msgParam}>|]


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

isMPType :: Decl -> Bool
isMPType MPType {..} = True
isMPType _ = False

genAlias :: Decl -> (T.Text, Type)
genAlias MPType {..} = (tyName, tyType)
genAlias _ = ("", TBool)

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

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|
// This file is auto-generated from #{filepath}
// *** DO NOT EDIT ***

#{content}

|]
