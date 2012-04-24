{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Ruby.Util where

import Data.Char
import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

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

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|# This file is auto-generated from #{filepath}
# *** DO NOT EDIT ***
#{content}
|]

snoc xs x = xs ++ [x]
