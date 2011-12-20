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
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath
    , configPackage :: String
    }
  deriving (Show, Eq)

generate :: Config -> Spec -> IO()
generate Config {..} spec = do
  let name = capitalize $ takeBaseName configFilePath
  LT.writeFile (name ++ "Client.java") $ templ configFilePath [lt|
package #{configPackage};
import java.util.ArrayList;
import org.msgpack.rpc.Client;
import org.msgpack.rpc.loop.EventLoop;
#{LT.concat $ map (genImport configPackage) spec}

#{LT.concat $ map genClient spec}
|]

  mapM_ (genStruct configPackage) spec
  mapM_ (genException configPackage) spec

  LT.writeFile (name ++ "Server.java") $ templ (configFilePath ++ configPackage ++"/server/")[lt|
import org.msgpack.rpc.Server;
package #{configPackage}

#{LT.concat $ map genServer spec}
|]

genImport :: FilePath -> Decl -> LT.Text
genImport packageName MPMessage {..} = 
    [lt|import #{packageName}.#{capitalizeT msgName};
|]
genImport _ _ = ""

genStruct :: FilePath -> Decl -> IO()
genStruct packageName MPMessage {..} = do
  let params = if null msgParam then "" else [lt|<#{T.intercalate ", " msgParam}>|]
  LT.writeFile ( (capitalize $ T.unpack msgName) ++ ".java") [lt|
package #{packageName};

public class #{capitalizeT msgName} #{params} {

#{LT.concat $ map genDecl msgFields}
  public #{capitalizeT msgName}() {
  #{LT.concat $ map genInit msgFields}
  }
};
|]

genStruct _ _ = return ()

genInit :: Field -> LT.Text
genInit Field {..} = case fldDefault of
                      Nothing -> ""
                      Just defaultVal -> [lt| #{fldName} = #{genLiteral defaultVal};|]

genDecl :: Field -> LT.Text
genDecl Field {..} = 
    [lt|  public #{genType fldType} #{fldName};
|]

genException :: FilePath -> Decl -> IO()
genException _ _ = return ()

genClient :: Decl -> LT.Text
genClient MPService {..} =
  [lt|
public class #{className} {
  public #{className}(String host, int port, double timeout_sec) throws Exception {
    EventLoop loop = EventLoop.defaultEventLoop();
    c_ = new Client(host, port, loop);
    iface_ = c_.proxy(RPCInterface.class);
  }

  public static interface RPCInterface {
#{LT.concat $ map genSignature serviceMethods}
  }

#{LT.concat $ map genMethodCall serviceMethods}
  private Client c_;
  private RPCInterface iface_;
}
|]
  where
  className = (capitalizeT serviceName) `mappend` "Client"
  genMethodCall Function {..} =
    let args = T.intercalate ", " $ pack methodArgs genArgs 
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
    where
      arg Field {..} = [lt|#{genType fldType} #{fldName}|]
  genMethodCall _ = ""

genClient _ = ""

genSignature :: Method -> LT.Text
genSignature Function {..} = 
    [lt|    #{genType methodRetType} #{methodName}(#{args});
|]
    where
      args = T.intercalate ", " $ pack methodArgs genArgs 
genSignature  _ = ""

genArgs :: Maybe Field -> T.Text
genArgs (Just ( Field {..}) ) = [st|#{genType fldType} #{fldName}|] 
genArgs Nothing = "Object x"

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

capitalizeT :: T.Text -> T.Text
capitalizeT a = T.cons (toUpper $ T.head a) (T.tail a)

capitalize :: String -> String
capitalize a = (toUpper $ head a) : (tail a)

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
  [lt|ArrayList<#{genType typ} >|]
genType (TMap typ1 typ2) =
  [lt|HashMap<#{genType typ1}, #{genType typ2} >|]
genType (TUserDef className params) =
  [lt|#{capitalizeT className} #{associateBracket $ map genType params}|]
genType (TTuple ts) =
  -- TODO: FIX
  foldr1 (\t1 t2 -> [lt|std::pair<#{t1}, #{t2} >|]) $ map genType ts
genType TObject =
  [lt|org.msgpack.type.Value|]
genType TVoid =
  [lt|void|]

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|
// This file is auto-generated from #{filepath}
// *** DO NOT EDIT ***

#{content}

|]
