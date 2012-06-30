import Control.Concurrent.Async
import Control.Monad.Trans

import Network (withSocketsDo)
import Network.MessagePackRpc.Server
import Network.MessagePackRpc.Client

main :: IO ()
main = withSocketsDo $ server `race_` client

server :: IO ()
server = serve 5000 [("add", fun add)] where
  add :: Int -> Int -> IO Int
  add x y = return $ x + y

client :: IO ()
client = runMPRPCClient "localhost" 5000 $ do
  liftIO . print =<< add 123 456
  where
    add :: Int -> Int -> MPRPC Int
    add = call "add"
