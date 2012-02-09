import Network.MessagePackRpc.Client

add :: RpcMethod (Int -> Int -> IO Int)
add  = method "add"

echo :: RpcMethod (String -> IO String)
echo = method "echo"

main :: IO ()
main = do
  conn <- connect "localhost" 8081
  print =<< add conn 123 456
  print =<< echo conn "hoge"
