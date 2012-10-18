{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Ruby (
  Config(..),
  generate,
  ) where

import Data.Char
import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text
import System.Directory

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath
    , configModule :: String
    }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} spec = do
  createDirectoryIfMissing True (takeBaseName configFilePath);
  setCurrentDirectory (takeBaseName configFilePath);
  let
        mods = LT.splitOn "::" $ LT.pack configModule
        
  LT.writeFile "types.rb" $ templ configFilePath [lt|
require 'rubygems'
require 'msgpack/rpc'
#{genModule mods $ LT.concat $ map (genTypeDecl "") spec }
|]
  
  LT.writeFile ("client.rb") $ templ configFilePath [lt|
require 'rubygems'
require 'msgpack/rpc'
require File.join(File.dirname(__FILE__), 'types')

#{genModule (snoc mods "Client") $ LT.concat $ map genClient spec}|]

genTypeDecl :: String -> Decl -> LT.Text
genTypeDecl _ MPType {..} = [lt|
class #{capitalizeT tyName}
  def #{capitalizeT tyName}.from_tuple(tuple)
    #{fromTuple tyType "tuple"}
  end
  def to_tuple(o)
    o
  end
end
|]

genTypeDecl _ MPMessage {..} =
  genMsg msgName msgFields False

genTypeDecl _ MPException {..} =
  genMsg excName excFields True
  
genTypeDecl _ _ = ""

genMsg :: T.Text -> [Field] -> Bool -> LT.Text
genMsg name flds isExc = [lt|
class #{capitalizeT name}#{deriveError}
  def initialize(#{T.intercalate ", " fs})
    #{LT.intercalate "\n    " $ map makeSubst fs}
  end
  def to_tuple    
    [#{LT.intercalate ",\n     " $ map make_tuple flds}]
  end
  def to_msgpack(out = '')
    to_tuple.to_msgpack(out)
  end
  def #{capitalizeT name}.from_tuple(tuple)
    #{capitalizeT name}.new(
      #{LT.intercalate ",\n      " $ map make_arg flds}
    )
  end
#{indent 2 $ genAccessors sorted_flds}
end
|]-- #{indent 2 $ LT.concat writers}
  where
    sorted_flds = sortField flds
    fs = map (maybe undefined fldName) sorted_flds
--    afs = LT.intercalate ",\n     " $ map make_tuple flds
    make_tuple Field {..} = 
      [lt|#{toTuple True fldType fldName}|]
    deriveError = if isExc then [lt| < StandardError|] else ""
    make_arg Field {..} =
      let fldIdstr = T.concat $ map T.pack ["tuple[", (show fldId), "]"]
      in [lt|#{fromTuple fldType fldIdstr}|]

makeSubst :: T.Text -> LT.Text
makeSubst fld = [lt| @#{fld} = #{fld} |]

toTuple :: Bool -> Type -> T.Text -> LT.Text
toTuple _ (TTuple ts) name = 
  let elems = map (f name) (zip [0..] ts) in
  [lt| [#{LT.concat elems}] |]
    where
      f :: T.Text -> (Integer, Type) -> LT.Text
      f n (i, (TUserDef _fg _ )) = [lt|#{n}[#{show i}].to_tuple}, |]
      f n (i, _) = [lt|#{n}[#{show i}], |]

toTuple True t name = [lt|@#{toTuple False t name}|]
toTuple _ (TNullable t) name = [lt|#{toTuple False t name}|]
toTuple _ (TInt _ _)    name = [lt|#{name}|]
toTuple _ (TFloat _)    name = [lt|#{name}|]
toTuple _ TBool         name = [lt|#{name}|]
toTuple _ TRaw          name = [lt|#{name}|]
toTuple _ TString       name = [lt|#{name}|]
toTuple _ (TList typ)   name = [lt|#{name}.map {|x| #{toTuple False typ "x"}}|]
toTuple _ (TMap typ1 typ2) name =
  [lt|#{name}.each_with_object({}) {|(k,v),h| h[#{toTuple False typ1 "k"}] = #{toTuple False typ2 "v"}}|]
toTuple _ (TUserDef _ _) name = [lt|#{name}.to_tuple|]

toTuple _ _ _ = ""

fromTuple :: Type -> T.Text -> LT.Text
fromTuple (TNullable t) name = [lt|#{fromTuple t name}|]
fromTuple (TInt _ _) name    = [lt|#{name}|]
fromTuple (TFloat _) name    = [lt|#{name}|]
fromTuple TBool name         = [lt|#{name}|]
fromTuple TRaw name          = [lt|#{name}|]
fromTuple TString name       = [lt|#{name}|]
fromTuple (TList typ) name =
  [lt|#{name}.map { |x| #{fromTuple typ "x"} }|]
  
fromTuple (TMap typ1 typ2) name =
  [lt|#{name}.each_with_object({}) {|(k,v),h| h[#{fromTuple typ1 "k"}] = #{fromTuple typ2 "v"} }|]

fromTuple (TUserDef className _) name = [lt|#{capitalizeT className}.from_tuple(#{name})|]

fromTuple (TTuple ts) name =
  let elems = map (f name) (zip [0..] ts) in
  [lt| [#{LT.intercalate ", " elems}] |]
    where
      f :: T.Text -> (Integer, Type) -> LT.Text
      f n (i, (TUserDef className _ )) = [lt|#{capitalizeT className}.from_tuple(#{n}[#{show i}]) |]
      f n (i, _) = [lt|#{n}[#{show i}] |]

fromTuple (TObject) name = [lt|#{name}|]

capitalizeT :: T.Text -> T.Text
capitalizeT a = T.cons (toUpper $ T.head a) (T.tail a)

sortField :: [Field] -> [Maybe Field]
sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix -> find ((==ix). fldId) flds

indent :: Int -> LT.Text -> LT.Text
indent ind lines = indentedConcat ind $ LT.lines lines

indentedConcat :: Int -> [LT.Text] -> LT.Text
indentedConcat ind lines =
  LT.dropAround (== '\n') $ LT.unlines $ map (indentLine ind) lines

indentLine :: Int -> LT.Text -> LT.Text
indentLine _ "" = ""
indentLine ind line = mappend (LT.pack $ replicate ind ' ') line

{-
extractJust :: [Maybe a] -> [a]
extractJust [] = []
extractJust (Nothing:xs) = extractJust xs
extractJust (Just v:xs)  = v : extractJust xs
-}

data AccessorType = Read | ReadWrite deriving Eq

getAccessorType :: Type -> AccessorType
getAccessorType TBool = Read
getAccessorType (TMap _ _) = Read
getAccessorType (TUserDef _ _) = Read
getAccessorType _ = ReadWrite

genAccessors :: [Maybe Field] -> LT.Text
genAccessors [] = ""
genAccessors fs = [lt|
#{genAccessors' Read "attr_reader" fs}#{genAccessors' ReadWrite "attr_accessor" fs}|]

genAccessors' :: AccessorType -> String -> [Maybe Field] -> LT.Text
genAccessors' at an flds = gen $ map (maybe undefined fldName) $ filter fldTypeEq flds
  where
    gen [] = ""
    gen fs = [lt|
#{an} #{T.intercalate ", " $ map (mappend ":") fs}|]

    fldTypeEq (Just Field {..}) = at == getAccessorType fldType
    fldTypeEq Nothing           = False


-- TODO: Check when val is not null with TNullable
-- TODO: Write single precision value on TFloat False
{-
genAttrWriter :: Field -> LT.Text
genAttrWriter Field {..} = genAttrWriter' fldType fldName

genAttrWriter' :: Type -> T.Text -> LT.Text

genAttrWriter' TBool n = [lt|
def #{n}=(val)
  @#{n} = val.to_b
end
|]

genAttrWriter' (TMap kt vt) n = [lt|
def #{n}=(val)
  @#{n} = {}
  val.each do |k, v|
#{indent 4 $ convert "k" "newk" kt}
#{indent 4 $ convert "v" "newv" vt}
  end
end
|]
  where
    convert from to (TUserDef t p) =
        genConvertingType from to (TUserDef t p)
    convert from to _ = [lt|#{to} = #{from}|]
genAttrWriter' (TUserDef name types) n = [lt|
def #{n}=(val)
#{indent 2 $ convert "val" atn (TUserDef name types)}
end
|]
  where
    atn = [lt|@#{n}|]
    convert from to (TUserDef t p) =
        genConvertingType from to (TUserDef t p)
genAttrWriter' _ _ = ""
-}


genClient :: Decl -> LT.Text
genClient MPService {..} = [lt|
class #{capitalizeT serviceName}
  def initialize(host, port)
    @cli = MessagePack::RPC::Client.new(host, port)
  end#{LT.concat $ map genMethodCall serviceMethods}
end
|]
  where
    genMethodCall Function {..} = [lt|
  def #{methodName}(#{defArgs})
#{indent 4 $ genConvertingType' callStr "v" methodRetType}
  end|]
      where
        defArgs = T.intercalate ", " $ map fldName methodArgs
        callStr = [lt|@cli.call(#{callArgs})|]
        callArgs = mappend ":" $ T.intercalate ", " $ methodName : sortedArgNames
        sortedArgNames = map (maybe undefined fldName) $ sortField methodArgs

genClient _ = ""

genConvertingType :: LT.Text -> LT.Text -> Type -> LT.Text
genConvertingType unpacked _ (TUserDef t _) = [lt|
#{capitalizeT t}.from_tuple(#{unpacked})|]
genConvertingType _ _ _ = ""

genConvertingType' :: LT.Text -> LT.Text -> Maybe Type -> LT.Text
genConvertingType' unpacked v (Just (TUserDef t p)) = [lt|
#{genConvertingType unpacked v (TUserDef t p)}
|]
genConvertingType' unpacked _ _ = [lt|#{unpacked}|]

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|# This file is auto-generated from #{filepath}
# *** DO NOT EDIT ***
#{content}
|]

genModule :: [LT.Text] -> LT.Text -> LT.Text
genModule modules content = f modules
  where
    f [] = [lt|#{content}|]
    f (n:ns) = [lt|module #{n}
#{f ns}
end|]

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]
