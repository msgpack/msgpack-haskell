{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Php (
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
import Data.Monoid

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
      
  LT.writeFile (name ++ "_types.php") $ templ configFilePath once "TYPES" [lt|
include_once 'Net/MessagePackRPC.php';

#{LT.concat $ map genTypeDecl spec}

class ObjectDecoder {
  public static $USER_DEFINED_CLASSES = array(
    #{LT.concat $ map genClassName spec}
  );
  public static function decodeToObject($ret_array, $type_array) {
    if ($type_array == "") {
      // do nothing
      $ret = $ret_array;
    } else if (in_array($type_array, self::$USER_DEFINED_CLASSES)) {
      // array -> object
      $ret = new $type_array();
      $ret_keys = array_keys((array)$ret);
      for ($i = 0; $i < count($ret_keys); $i++) {
        $ret->{$ret_keys[$i]} = $ret_array[$i];
      }
    } else {
      // dissolve array
      if (is_array($type_array)) {
        if (count($type_array) == 1) {
          // if array
          foreach ($type_array as $key => $type) {
            foreach ($ret_array as $ret_key => $ret_value) {
              $ret[$ret_key] = $this->decodeToObject($ret_value, $type);
            }
          }
        } else {
          // if tuple
          $ret = array();
          $i = 0;
          foreach ($type_array as $type) {
            $ret[$i] = $this->decodeToObject($ret_array[$i], $type);
            $i++;
          }
        }
      } else {
        // type error
        return $ret_array;
      }
    }
    return $ret;
  }
}
 
|]

  LT.writeFile (name ++ "_client.php") [lt|
<?php
include_once(dirname(__FILE__)."/#{name}_types.php");

#{LT.concat $ map genClient spec}
?>
|]

genClassName :: Decl -> LT.Text
genClassName MPMessage {..} =
  [lt|  "#{msgName}",
  |]  
genClassName _ = ""

genTypeDecl :: Decl -> LT.Text
genTypeDecl MPMessage {..} =
  genMsg msgName msgFields False

genTypeDecl MPException {..} =
  genMsg excName excFields True

genTypeDecl _ = ""

genMsg name flds isExc =
  let fields = map f flds
      fs = map (maybe undefined fldName) $ sortField flds
  in [lt|
class #{name}#{e} {

#{LT.concat fields}
}
|]
  where
    e = if isExc then [lt| extends Exception|] else ""

    f Field {..} = [lt| public $#{fldName};
|]

sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix ->
  find ((==ix). fldId) flds

genClient :: Decl -> LT.Text
genClient MPService {..} = [lt|
class #{serviceName} {
  public function __construct($host, $port) {
    $this->client = new MessagePackRPC_Client($host, $port);
  }
#{LT.concat $ map genMethodCall serviceMethods}
  private $client;
}
|]
  where
  genMethodCall Function {..} =
    let args = LT.intercalate ", " $ map arg methodArgs in
    let sortedArgs = LT.intercalate ", " $ map (maybe undefined arg) $ sortField methodArgs in
    case methodRetType of
      Nothing -> [lt|
  public function #{methodName}(#{args}) {
    $this->client->call("#{methodName}", array(#{sortedArgs}));
  }
|]
      Just typ -> [lt|
  public function #{methodName}(#{args}) {
    $ret = $this->client->call("#{methodName}", array(#{sortedArgs}));
    $type_array = #{genTypeArray typ};
    return ObjectDecoder::decodeToObject($ret, $type_array);
  }
|]
    where
      arg Field {..} = [lt|$#{fldName}|]

  genMethodCall _ = ""
  
genClient _ = ""

genTypeArray :: Type -> LT.Text
genTypeArray (TList typ) =
  [lt|array(#{genTypeArray typ})|]
genTypeArray (TMap typ1 typ2) =
  [lt|array(#{genTypeArray typ1} => #{genTypeArray typ2})|]
genTypeArray (TUserDef className params) =
  [lt|"#{className}"|]
genTypeArray (TTuple ts) =
  foldr1 (\t1 t2 -> [lt|array(#{t1}, #{t2})|]) $ map genTypeArray ts
genTypeArray _ = [lt|""|]

genType :: Type -> LT.Text
genType (TUserDef className params) =
  [lt|#{className}|]
genType _ = ""

templ :: FilePath -> String -> String -> LT.Text -> LT.Text
templ filepath once name content = [lt|
// This file is auto-generated from #{filepath}
// *** DO NOT EDIT ***
<?php
#{content}
?>
|]
    
snoc xs x = xs ++ [x]
