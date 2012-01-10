{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Perl (
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

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath
    , configNameSpace :: String
    }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} spec = do
  let name = takeBaseName configFilePath
      once = map toUpper name
      ns = LT.splitOn "::" $ LT.pack configNameSpace

  LT.writeFile (name ++ "_types.pm") $ templ configFilePath once "TYPES" [lt|
package types;
use strict;
use warnings;

#{LT.concat $ map (genTypeDecl name) spec }
|]

  LT.writeFile (name ++ "_client.pm") [lt|
package #{name}_client;
use strict;
use warnings;
use AnyEvent::MPRPC::Client;
#{LT.concat $ map genClient spec}
|]

genTypeDecl :: String -> Decl -> LT.Text
genTypeDecl _ MPMessage {..} =
  genMsg msgName msgFields False

genTypeDecl _ MPException {..} =
  genMsg excName excFields True
  
genTypeDecl _ MPType { .. } =
  [lt|
typedef #{genType tyType} #{tyName};
|]

genTypeDecl _ _ = ""

genMsg name flds isExc =
  let fields = map f flds
      fs = map (maybe undefined fldName) $ sortField flds
  in [lt|
struct #{name} {
#{LT.concat fields}
};
|]
  where
    f Field {..} = [lt|
  #{genType fldType} #{fldName};|]

sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix ->
  find ((==ix). fldId) flds

genClient :: Decl -> LT.Text
genClient MPService {..} = [lt|
sub new {
  my ($self, $host, $port) = @_;
  my $client = AnyEvent::MPRPC::Client->new(
    host => $host,
    port => $port
    );
  bless { client => $client }, $self;
};

sub bar {
  my ($self, $lang, $xs) = @_;
  $self->{'client'}->call(bar => [$xs, $lang])->recv;
};

1;
|]
  where
  genMethodCall Function {..} =
    let args = LT.intercalate ", " $ map arg methodArgs in
    let vals = LT.concat $ map val methodArgs in
    [lt|
    #{genType methodRetType} #{methodName}(#{args}) {
      return c_.call("#{methodName}"#{vals}).get<#{genType methodRetType} >();
    }
|]
    where
      arg Field {..} = [lt|#{genType fldType} #{fldName}|]
      val Field {..} = [lt|, #{fldName}|]

  genMethodCall _ = ""

genClient _ = ""

genType :: Type -> LT.Text
genType (TInt sign bits) =
  let base = if sign then "int" else "uint" :: LT.Text in
  [lt|#{base}#{show bits}_t|]
genType (TFloat False) =
  [lt|float|]
genType (TFloat True) =
  [lt|double|]
genType TBool =
  [lt|bool|]
genType TRaw =
  [lt|std::string|]
genType TString =
  [lt|std::string|]
genType (TList typ) =
  [lt|std::vector<#{genType typ} >|]
genType (TMap typ1 typ2) =
  [lt|std::map<#{genType typ1}, #{genType typ2} >|]
genType (TUserDef className params) =
  [lt|#{className}|]
genType (TTuple ts) =
  -- TODO: FIX
  foldr1 (\t1 t2 -> [lt|std::pair<#{t1}, #{t2} >|]) $ map genType ts
genType TObject =
  [lt|msgpack::object|]
genType TVoid =
  [lt|void|]

templ :: FilePath -> String -> String -> LT.Text -> LT.Text
templ filepath once name content = [lt|
// This file is auto-generated from #{filepath}
// *** DO NOT EDIT ***

#{content}

|]
