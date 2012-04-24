{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Ruby.Module (
  genModule
  ) where

import qualified Data.Text.Lazy as LT
import Text.Shakespeare.Text

genModule :: [LT.Text] -> LT.Text -> LT.Text
genModule modules content = f modules
  where
    f [] = [lt|#{content}|]
    f (n:ns) = [lt|module #{n}
#{f ns}
end|]

