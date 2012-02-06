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

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath
    , configModule :: String
    }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} spec = do
  let name = takeBaseName configFilePath
      once = map toUpper name
      mods = LT.splitOn "::" $ LT.pack configModule
  LT.writeFile (name ++ "_types.rb") $ templ configFilePath [lt|
require 'msgpack/rpc'

#{genModule mods $ LT.concat $ map (genTypeDecl name) spec }|]
  
  LT.writeFile (name ++ "_client.rb") $ templ configFilePath [lt|
require 'msgpack/rpc'
require './#{name}_types'

#{genModule (snoc mods "Client") $ LT.concat $ map genClient spec}|]

genTypeDecl :: String -> Decl -> LT.Text
genTypeDecl _ MPMessage {..} =
  genMsg msgName msgFields False

genTypeDecl _ MPException {..} =
  genMsg excName excFields True
  
genTypeDecl _ _ = ""

genMsg name flds isExc = [lt|
class #{capitalizeT name}#{deriveError}
  def to_msgpack(out = '')
    [#{afs}].to_msgpack(out)
  end

  def from_unpacked(unpacked)
    #{afs} = unpacked
  end

#{indent 2 $ LT.concat writers}

#{indent 2 $ genAccessors sorted_flds}
end
|]
  where
    sorted_flds = sortField flds
    fs = map (maybe undefined fldName) sorted_flds
    writers = map (maybe undefined genAttrWriter) sorted_flds
    afs = T.intercalate ", " $ map (mappend "@") fs
    deriveError = if isExc then [lt| < StandardError|] else ""

capitalizeT :: T.Text -> T.Text
capitalizeT a = T.cons (toUpper $ T.head a) (T.tail a)

sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix -> find ((==ix). fldId) flds

indent :: Int -> LT.Text -> LT.Text
indent ind lines = indentedConcat ind $ LT.lines lines

indentedConcat :: Int -> [LT.Text] -> LT.Text
indentedConcat ind lines =
  LT.dropAround (== '\n') $ LT.unlines $ map (indentLine ind) lines

indentLine :: Int -> LT.Text -> LT.Text
indentLine ind "" = ""
indentLine ind line = mappend (LT.pack $ replicate ind ' ') line

extractJust :: [Maybe a] -> [a]
extractJust [] = []
extractJust (Nothing:xs) = extractJust xs
extractJust (Just v:xs)  = v : extractJust xs

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
genConvertingType unpacked v (TUserDef t _) = [lt|
#{v} = #{capitalizeT t}.new
#{v}.from_unpacked(#{unpacked})|]
genConvertingType _ _ _ = ""

genConvertingType' :: LT.Text -> LT.Text -> Type -> LT.Text
genConvertingType' unpacked v (TUserDef t p) = [lt|
#{genConvertingType unpacked v (TUserDef t p)}
return v|]
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

snoc xs x = xs ++ [x]
