{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE Trustworthy #-}
module Data.MessagePack.ResultSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Applicative           (empty, pure, (<$>), (<*>),
                                                (<|>))
import qualified Data.MessagePack.Types.Result as R


newtype F = F (Int -> R.Result Int)

instance Show F where
  show = const "<function>"

instance Arbitrary F where
  arbitrary = F <$> arbitrary


-- | Checks that 'R.Result' satisfies the laws described in the 'Monad' and
-- 'Applicative' documentation.
--
-- Also see:
-- https://wiki.haskell.org/Monad_laws
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html#t:Applicative
spec :: Spec
spec = do
  describe "Monad" $ do
    it "satisfies left identity" $
      property $ \a (F f) ->
        (return' a `bind'` f) `shouldBe` f a

    it "satisfies right identity" $
      property $ \m ->
        (m `bind'` return') `shouldBe` m

    it "satisfies associativity" $
      property $ \m (F f) (F g) ->
        ((m `bind'` f) `bind'` g) `shouldBe` (m `bind'` (\x -> f x `bind'` g))

    it "supports 'fail'" $
      fail' "nope" `shouldBe` R.Failure "nope"

  describe "Applicative" $ do
    it "satisfies identity" $
      property identity

    it "satisfies composition" $
      property $ \x y w -> do
        composition (R.Failure "nope") (R.Failure "no way") w
        composition (R.Failure "nope") (pure (y *)        ) w
        composition (pure (x *)      ) (R.Failure "no way") w
        composition (pure (x *)      ) (pure (y *)        ) w

    it "satisfies homomorphism" $
      property $ \x -> homomorphism (x *)

    it "satisfies interchange" $
      property $ \x y -> do
        interchange (R.Failure "nope") y
        interchange (pure (x *)      ) y

  describe "Alternative" $ do
    it "chooses the left-most success" $ do
      R.Success "a" <|> R.Success "b" `shouldBe` R.Success "a"
      R.Success "a" <|> R.Failure "b" `shouldBe` R.Success "a"
      R.Failure "a" <|> R.Success "b" `shouldBe` R.Success "b"

    it "chooses the right-most failure" $
      R.Failure "a" <|> R.Failure "b" `shouldBe` (R.Failure "b" :: R.Result ())

    describe "empty" $
      it "is a failure" $
        empty <|> R.Success "a" `shouldBe` R.Success "a"

  where
    --
    -- Aliases constrained to the Result monad. These also help avoid lint
    -- warnings about using monad laws.
    --

    return' :: Int -> R.Result Int
    return' = return

    bind' :: R.Result Int -> (Int -> R.Result Int) -> R.Result Int
    bind' = (>>=)

    fail' :: String -> R.Result Int
    fail' = fail

    pure' :: a -> R.Result a
    pure' = pure

    --
    -- Applicative laws.
    --

    identity :: R.Result Int -> Expectation
    identity v =
      (pure' id <*> v) `shouldBe` v

    composition :: R.Result (Int -> Int) -> R.Result (Int -> Int) -> R.Result Int -> Expectation
    composition u v w =
      (pure' (.) <*> u <*> v <*> w) `shouldBe` (u <*> (v <*> w))

    homomorphism :: (Int -> Int) -> Int -> Expectation
    homomorphism h x =
      (pure' h <*> pure' x) `shouldBe` pure' (h x)

    interchange :: R.Result (Int -> Int) -> Int -> Expectation
    interchange u y =
      (u <*> pure' y) `shouldBe` (pure' ($ y) <*> u)
