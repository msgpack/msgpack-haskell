{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE Trustworthy #-}
module Data.MessagePack.OptionSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Applicative           (empty, pure, (<$>), (<*>),
                                                (<|>))
import           Control.Monad                 (mplus, mzero)
import qualified Data.MessagePack.Types.Option as O


newtype F = F (Int -> O.Option Int)

instance Show F where
  show = const "<function>"

instance Arbitrary F where
  arbitrary = F <$> arbitrary


-- | Checks that 'O.Option' satisfies the laws described in the 'Monad' and
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
      fail' "nope" `shouldBe` O.None

  describe "Applicative" $ do
    it "satisfies identity" $
      property identity

    it "satisfies composition" $
      property $ \x y w -> do
        composition  O.None             O.None              w
        composition  O.None            (pure (y *)        ) w
        composition (pure (x *)      )  O.None              w
        composition (pure (x *)      ) (pure (y *)        ) w

    it "satisfies homomorphism" $
      property $ \x -> homomorphism (x *)

    it "satisfies interchange" $
      property $ \x y -> do
        interchange  O.None            y
        interchange (pure (x *)      ) y

  describe "Alternative" $ do
    it "chooses the left-most success" $ do
      O.Some "a" <|> O.Some "b" `shouldBe` O.Some "a"
      O.Some "a" <|> O.None     `shouldBe` O.Some "a"
      O.None     <|> O.Some "b" `shouldBe` O.Some "b"

    it "chooses the right-most failure" $
      O.None        <|> O.None        `shouldBe` (O.None :: O.Option ())

    describe "empty" $
      it "is a failure" $
        empty <|> O.Some "a" `shouldBe` O.Some "a"

  describe "MonadPlus" $ do
    it "chooses the left-most success" $ do
      O.Some "a" `mplus` O.Some "b" `shouldBe` O.Some "a"
      O.Some "a" `mplus` O.None     `shouldBe` O.Some "a"
      O.None     `mplus` O.Some "b" `shouldBe` O.Some "b"

    it "chooses the right-most failure" $
      O.None     `mplus` O.None        `shouldBe` (O.None :: O.Option ())

    describe "mzero" $
      it "is a failure" $
        mzero `mplus` O.Some "a" `shouldBe` O.Some "a"

  where
    --
    -- Aliases constrained to the Option monad. These also help avoid lint
    -- warnings about using monad laws.
    --

    return' :: Int -> O.Option Int
    return' = return

    bind' :: O.Option Int -> (Int -> O.Option Int) -> O.Option Int
    bind' = (>>=)

    fail' :: String -> O.Option Int
    fail' = fail

    pure' :: a -> O.Option a
    pure' = pure

    --
    -- Applicative laws.
    --

    identity :: O.Option Int -> Expectation
    identity v =
      (pure' id <*> v) `shouldBe` v

    composition :: O.Option (Int -> Int) -> O.Option (Int -> Int) -> O.Option Int -> Expectation
    composition u v w =
      (pure' (.) <*> u <*> v <*> w) `shouldBe` (u <*> (v <*> w))

    homomorphism :: (Int -> Int) -> Int -> Expectation
    homomorphism h x =
      (pure' h <*> pure' x) `shouldBe` pure' (h x)

    interchange :: O.Option (Int -> Int) -> Int -> Expectation
    interchange u y =
      (u <*> pure' y) `shouldBe` (pure' ($ y) <*> u)
