{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Cpp.NameSpace (
  genNameSpace
  ) where

import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text

genNameSpace :: [LT.Text] -> LT.Text -> LT.Text
genNameSpace namespace content = f namespace
  where
    f [] = [lt|#{content}|]
    f (n:ns) = [lt|
namespace #{n} {
#{f ns}
} // namespace #{n}
|]

