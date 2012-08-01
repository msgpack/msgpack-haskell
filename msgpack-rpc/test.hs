import Control.Applicative
import Control.Concurrent.Async
import Control.Monad.Trans

import Test.Hspec

import Network (withSocketsDo)
import Network.MessagePackRpc.Server
import Network.MessagePackRpc.Client

port :: Int
port = 5000

main :: IO ()
main = withSocketsDo $ hspec $ do
  describe "add service" $ do
    it "correct" $ do
      server `race_` client

server :: IO ()
server = serve port [("add", toMethod add)]
  where
    add :: Int -> Int -> Method Int
    add x y = return $ x + y

client :: IO ()
client = runClient "localhost" port $ do
  ret <- add 123 456
  liftIO $ ret `shouldBe` 123 + 456
  where
    add :: Int -> Int -> Client Int
    add = call "add"
