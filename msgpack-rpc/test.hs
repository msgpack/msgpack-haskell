import Control.Concurrent.Async
import Control.Monad.Trans

import Network (withSocketsDo)
import Network.MessagePackRpc.Server
import Network.MessagePackRpc.Client

main :: IO ()
main = withSocketsDo $ server `race_` client

add :: Int -> Int -> IO Int
add x y = return $ x + y

server :: IO ()
server = serve 5000 [("add", fun add)]

client :: IO ()
client = runMPRPCClient "localhost" 5000 $ do
  res <- call "add" (123 :: Int) (456 :: Int)
  liftIO $ print (res :: Int)
