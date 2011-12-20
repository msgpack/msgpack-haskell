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
class #{capitalizeT name}#{e}
  def to_msgpack(out = '')
    [#{afs}].to_msgpack(out)
  end

  def from_unpacked(unpacked)
    #{afs} = unpacked
  end

#{indent 2 $ LT.unlines $ map (maybe undefined genAttrWriter) sorted_flds}

#{indent 2 $ genReaders fs}
end
|]
  where
    sorted_flds = sortField flds
    fs = map (maybe undefined fldName) sorted_flds
    afs = T.intercalate ", " $ map (mappend "@") fs
    e  = if isExc then [lt| < Exception|] else ""

capitalizeT :: T.Text -> T.Text
capitalizeT a = T.cons (toUpper $ T.head a) (T.tail a)

sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix -> find ((==ix). fldId) flds

indentedConcat :: Int -> [LT.Text] -> LT.Text
indentedConcat ind lines =
  LT.dropAround (== '\n') $ LT.unlines $ map (indentLine ind) lines

indentLine :: Int -> LT.Text -> LT.Text
indentLine ind "" = ""
indentLine ind line = mappend (LT.pack $ replicate ind ' ') line

indent :: Int -> LT.Text -> LT.Text
indent ind lines = indentedConcat ind $ LT.lines lines

genReaders [] = ""
genReaders fs = [lt|attr_reader #{T.intercalate ", " $ map (mappend ":") fs}|]

genAttrWriter Field {..} = [lt|
def #{fldName}= val
#{indent 2 $ genAttrWriter' fldType $ LT.fromStrict fldName}
end|]

genAttrWriter' :: Type -> LT.Text -> LT.Text
genAttrWriter' (TInt t v) n =
  [lt|#{genCheckingValue (TInt t v) "val"}#{genDefaultAttrWriter n}|]
-- TODO: Check if val is either single or double precision
genAttrWriter' (TFloat _) n = genDefaultAttrWriter n
genAttrWriter' TBool n = [lt|@#{n} = val.to_b|]
genAttrWriter' TRaw n = genDefaultAttrWriter n
genAttrWriter' TString n = genDefaultAttrWriter n
-- TODO: Check when val is not null
genAttrWriter' (TNullable t) n = genDefaultAttrWriter n
genAttrWriter' (TList t) n = [lt|#{n}.each do |i| #{genCheckingValue t "val[i]"} end
#{genDefaultAttrWriter n}|]
genAttrWriter' (TMap kt vt) n =
  [lt|#{n} = {}
val.each do |k, v|
#{indent 2 $ convert "k" "newk" kt}
#{indent 2 $ convert "v" "newv" vt}
#{indent 2 $ genCheckingValue kt "newk"}
#{indent 2 $ genCheckingValue vt "newv"}
  #{n}[newk] = newv
end|]
  where
    convert from to (TUserDef t p) =
        genConvertingType from to (TUserDef t p)
    convert from to _ = [lt|#{to} = #{from}|]

genAttrWriter' (TTuple ts) n =
  [lt|#{genCheckingTuple ts 0 "val"}#{genDefaultAttrWriter n}|]
genAttrWriter' (TUserDef _ _) n = genDefaultAttrWriter n
genAttrWriter' _ n = genDefaultAttrWriter n

genCheckingValue :: Type -> LT.Text -> LT.Text
genCheckingValue (TInt True v) n =
  [lt|raise if #{n} < -#{show (2 ^ (v - 1))} || #{n} >= #{show (2 ^ (v - 1))}
|]
genCheckingValue (TInt False v) n =
  [lt|raise if #{n} < 0 || #{n} >= #{show (2 ^ v)}
|]
genCheckingValue _ _ = ""

genCheckingTuple :: [Type] -> Int -> LT.Text -> LT.Text
genCheckingTuple [] _ _ = ""
genCheckingTuple (t:ts) i n =
  [lt|#{genCheckingValue t $ genIndexedValue i n}#{genCheckingTuple ts (i + 1) n}|]
genIndexedValue i n = [lt|#{n}[#{show i}]|]

genDefaultAttrWriter n = [lt|@#{n} = val|]

genClient :: Decl -> LT.Text
genClient MPService {..} = [lt|
class #{capitalizeT serviceName}
  def initialize(host, port)
    @cli = MessagePack::RPC::Client.new(host, port)
  end
#{LT.concat $ map genMethodCall serviceMethods}
end
|]
  where
    genMethodCall Function {..} = [lt|
  def #{methodName}(#{defArgs})
#{genCheckingArgs methodArgs}#{genConvertingType' callStr "v" methodRetType}
  end
|]
      where
        defArgs = T.intercalate ", " $ map fldName methodArgs
        callStr = [lt|@cli.call(#{callArgs})|]
        callArgs = mappend ":" $ T.intercalate ", " $ methodName : sortedArgNames
        sortedArgNames = map (maybe undefined fldName) $ sortField methodArgs

genClient _ = ""

genCheckingArgs :: [Field] -> LT.Text
genCheckingArgs [] = ""
genCheckingArgs (f:fs) = [lt|#{genCheckingArg f}#{genCheckingArgs fs}|]

genCheckingArg Field {..} = genCheckingValue fldType [lt|#{fldName}|]

genConvertingType :: LT.Text -> LT.Text -> Type -> LT.Text
genConvertingType unpacked v (TUserDef t _) =
  [lt|#{v} = #{capitalizeT t}.new
#{v}.from_unpacked(#{unpacked})|]
genConvertingType _ _ _ = ""

genConvertingType' :: LT.Text -> LT.Text -> Type -> LT.Text
genConvertingType' unpacked v (TUserDef t p) =
  [lt|#{genConvertingType unpacked v (TUserDef t p)}
return v|]
genConvertingType' unpacked _ _ = [lt|#{unpacked}|]

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content =
  [lt|# This file is auto-generated from #{filepath}
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
