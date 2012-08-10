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
server =
  serve port
    [ ("add", toMethod add)
    , ("echo", toMethod echo)
    ]
  where
    add :: Int -> Int -> Method Int
    add x y = return $ x + y

    echo :: String -> Method String
    echo s = return $ "***" ++ s ++ "***"

client :: IO ()
client = runClient "localhost" port $ do
  r1 <- add 123 456
  liftIO $ r1 `shouldBe` 123 + 456
  r2 <- echo "hello"
  liftIO $ r2 `shouldBe` "***hello***"
  where
    add :: Int -> Int -> Client Int
    add = call "add"

    echo :: String -> Client String
    echo = call "echo"
