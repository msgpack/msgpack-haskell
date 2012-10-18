{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.MessagePack.IDL.CodeGen.Haskell (
  Config(..),
  generate,
  ) where

import Data.Char
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax as MP

data Config
  = Config
    { configFilePath :: FilePath
    }

generate :: Config -> Spec -> IO ()
generate Config {..} spec = do
  LT.writeFile "Types.hs" [lt|
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Int
import Data.MessagePack
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Words
#{LT.concat $ map genTypeDecl spec}
|]

  LT.writeFile "Server.hs" [lt|
|]

  LT.writeFile "Client.hs" [lt|
module Server where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import qualified Network.MessagePackRpc.Client as MP

import Types
#{LT.concat $ map genClient spec}
|]

genClient :: Decl -> LT.Text
genClient MPService {..} =
  [lt|
newtype #{monadName} m a
  = #{monadName} { un#{monadName} :: StateT () m a }
  deriving (Monad, MonadIO, MonadTrans, MonadState ())
#{LT.concat $ map genMethod serviceMethods}
|]
  where
    monadName = classize (serviceName) `mappend` "T"
    genMethod Function {..} =
      let ts = map (genType . fldType) methodArgs in
      let typs = ts ++ [ [lt|#{monadName} (#{genRetType methodRetType})|] ] in
      [lt|
#{methodize methodName} :: #{LT.intercalate " -> " typs}
#{methodize methodName} = MP.method "#{methodName}"
|]
    genMethod f = error $ "unsupported: " ++ show f

genClient _ = ""

genTypeDecl :: Decl -> LT.Text
genTypeDecl MPMessage {..} =
  let mems = LT.intercalate "\n  , " $ map f msgFields in
  [lt|
data #{dataName}
  = #{dataName}
  { #{mems}
  }
  deriving (Eq, Show)
deriveObject False ''#{dataName}
|]
  where
    dataName = classize msgName
    f Field {..} =
      let fname = uncapital dataName `mappend` (capital $ camelize fldName) in
      [lt|#{fname} :: #{genType fldType}|]

genTypeDecl _ = ""

genType :: Type -> LT.Text
genType (TInt sign bits) =
  let base = if sign then "Int" else "Word" :: T.Text in
  [lt|#{base}#{show bits}|]
genType (TFloat False) =
  [lt|Float|]
genType (TFloat True) =
  [lt|Double|]
genType TBool =
  [lt|Bool|]
genType TRaw =
  [lt|ByteString|]
genType TString =
  [lt|Text|]
genType (TList typ) =
  [lt|[#{genType typ}]|]
genType (TMap typ1 typ2) =
  [lt|Map (#{genType typ1}) (#{genType typ2})|]
genType (TTuple typs) =
  [lt|(#{LT.intercalate ", " $ map genType typs})|]
genType (TUserDef name params) =
  [lt|#{classize name}|]
genType (TObject) =
  undefined

genRetType :: Maybe Type -> LT.Text
genRetType Nothing = "()"
genRetType (Just t) = genType t

classize :: T.Text -> T.Text
classize = capital . camelize

methodize :: T.Text -> T.Text
methodize = uncapital . camelize

camelize :: T.Text -> T.Text
camelize = T.concat . map capital . T.words . T.map ubToSpc where
  ubToSpc '_' = ' '
  ubToSpc c = c

capital :: T.Text -> T.Text
capital word =
  (T.map toUpper $ T.take 1 word) `mappend` T.drop 1 word

uncapital :: T.Text -> T.Text
uncapital word =
  (T.map toLower $ T.take 1 word) `mappend` T.drop 1 word

{-
genServer :: Spec -> IO Builder
genServer = undefined

genClient :: Spec -> IO Builder
genClient spec = do
  decs <- runQ $ genClient' spec
  putStrLn $ pprint decs
  undefined

genClient' :: Spec -> Q [Dec]
genClient' spec = return . concat =<< mapM genDecl spec

genDecl :: Decl -> Q [Dec]
genDecl (Message name super fields) = do
  let clsName = mkName $ T.unpack name
      con = recC clsName $ map genFld fields
  d <- dataD (cxt []) clsName [] [con] [''Eq, ''Ord, ''Show]
  return [d]
  where
    genFld (Field fid req typ fname _) =
      varStrictType (mkName $ uncapital $ T.unpack name ++ capital (T.unpack fname)) (strictType notStrict $ genType typ)

genDecl (Service name version meths) = do
  return []

genDecl _ = do
  d <- dataD (cxt []) (mkName "Ign") [] [] []
  return [d]

genType :: MP.Type -> Q TH.Type
genType (TInt False 8 ) = conT ''Word8
genType (TInt False 16) = conT ''Word16
genType (TInt False 32) = conT ''Word32
genType (TInt False 64) = conT ''Word64
genType (TInt True  8 ) = conT ''Int8
genType (TInt True  16) = conT ''Int16
genType (TInt True  32) = conT ''Int32
genType (TInt True  64) = conT ''Int64

genType (TFloat False) = conT ''Float
genType (TFloat True ) = conT ''Double

genType TBool = conT ''Bool
genType TRaw = conT ''B.ByteString
genType TString = conT ''T.Text

genType (TList typ) =
  listT `appT` genType typ
genType (TMap kt vt) =
  [t| M.Map $(genType kt) $(genType vt) |]

genType (TClass name) =
  conT $ mkName $ capital $ T.unpack name

genType (TTuple typs) =
  foldl appT (tupleT (length typs)) (map genType typs)

capital (c:cs) = toUpper c : cs
capital cs = cs

uncapital (c:cs) = toLower c : cs
uncapital cs = cs
-}
