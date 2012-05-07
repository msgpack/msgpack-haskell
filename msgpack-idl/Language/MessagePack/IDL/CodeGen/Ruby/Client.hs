{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Ruby.Client (
  genClientMain
  ) where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax
import Language.MessagePack.IDL.CodeGen.Ruby.Util
import Language.MessagePack.IDL.CodeGen.Ruby.Config
import Language.MessagePack.IDL.CodeGen.Ruby.Module
import Language.MessagePack.IDL.CodeGen.Ruby.ConvertingType

genClientMain name once mods Config {..} spec = LT.writeFile (name ++ "_client.rb") $ templ configFilePath [lt|
require 'msgpack/rpc'
require './#{name}_types'

#{genModule (snoc mods "Client") $ LT.concat $ map genClient spec}|]

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
