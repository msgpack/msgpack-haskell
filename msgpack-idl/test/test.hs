import Test.Hspec.Monadic

main :: IO ()
main = hspecX $ do
  describe "parser" $ do
    it "can parse xxx..." $ do
      pending

  describe "checker" $ do
    it "can check xxx..." $ do
      pending

  describe "generator" $ do
    describe "haskell" $ do
      it "can generate client" $ do
        pending
      it "can communicate reference server" $ do
        pending
