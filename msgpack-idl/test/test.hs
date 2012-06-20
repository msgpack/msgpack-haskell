{-# LANGUAGE QuasiQuotes #-}
import Data.ListLike.Text ()
import qualified Data.Text as T
import Test.Hspec.Monadic hiding (Spec)
import Test.Hspec.HUnit
import Test.HUnit
import Text.Peggy
import Text.Shakespeare.Text

import Language.MessagePack.IDL
import Language.MessagePack.IDL.Check

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "can parse xxx..." $ do
      pending

  describe "checker" $ do
    it "find multiple decl of types" $ do
      checkIDL "multiple msg" [st|
message hoge {}
message hoge {}
|]
      checkIDL "mixed" [st|
message hoge {}
exception hoge {}
|]

    it "find nub id" $ do
      checkIDL "conflict ids" [st|
message hoge {
  0: string hoge
  0: string moge
}
|]
      checkIDL "conflict names" [st|
message hoge {
  0: string hoge
  2: string hoge
}
|]

  describe "generator" $ do
    describe "haskell" $ do
      it "can generate client" $ do
        pending
      it "can communicate reference server" $ do
        pending

checkIDL :: String -> T.Text -> Assertion
checkIDL msg = assertBool msg . isLeft . check . parseIDL

-- Currently, peggy's QQ generator seems not to work correctly.
-- So impl parse function.
parseIDL :: T.Text -> Spec
parseIDL txt = case parse idl (SrcPos "" 0 0 0) txt of
  Left err -> error $ show err
  Right spec -> spec

isLeft (Left _) = True
isLeft _ = False
