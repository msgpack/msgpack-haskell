{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Cpp.NameSpace (
  genNameSpace
  ) where

import qualified Data.Text.Lazy as LT
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

