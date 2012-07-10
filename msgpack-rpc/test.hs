import Control.Applicative
import Control.Concurrent.Async
import Control.Monad.Trans

import Test.Hspec.HUnit ()
import Test.Hspec.Monadic
import Test.HUnit
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property

import Network (withSocketsDo)
import Network.MessagePackRpc.Server
import Network.MessagePackRpc.Client

main :: IO ()
main = hspec $ do
  describe "add service" $ do
    it "correct" $ withSocketsDo $ do
      server `race_` client

server :: IO ()
server = serve 5000 [("add", fun add)]
  where
    add :: Int -> Int -> MethodT IO Int
    add x y = MethodT $ return $ x + y

client :: IO Property
client = runMPRPCClient "localhost" 5000 $ monadic' $ do -- Gen (MPRPC Property)
  x <- pick arbitrary
  y <- pick arbitrary
  ret <- run $ add x y
  return $ ret == x + y :: PropertyM MPRPC Bool
  where
    add :: Int -> Int -> MPRPC Int
    add = call "add"
