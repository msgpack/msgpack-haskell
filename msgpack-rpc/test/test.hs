{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Trans
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.MessagePack.Client
import           Network.MessagePack.Server
import           Network.Socket             (withSocketsDo)

import           System.IO                  (openTempFile)

port :: Int
port = 5000

main :: IO ()
main = withSocketsDo $ defaultMain $
  testGroup "simple service"
  [ testCase "test TCP" $ serverTCP `race_` (threadDelay 1000 >> clientTCP)
  , testCase "test Unix" unixCase]

unixCase :: IO ()
unixCase = do
  (f, _) <- openTempFile "/tmp" "socket.sock"
  serverUnix f `race_` (threadDelay 1000 >> clientUnix f)

serverTCP :: IO ()
serverTCP =
  serve port
    [ method "add"  add
    , method "echo" echo
    ]
  where
    add :: Int -> Int -> Server Int
    add x y = return $ x + y

    echo :: String -> Server String
    echo s = return $ "***" ++ s ++ "***"

clientTCP :: IO ()
clientTCP = execClient "localhost" port $ do
  r1 <- add 123 456
  liftIO $ r1 @?= 123 + 456
  r2 <- echo "hello"
  liftIO $ r2 @?= "***hello***"
  where
    add :: Int -> Int -> Client Int
    add = call "add"

    echo :: String -> Client String
    echo = call "echo"

serverUnix :: FilePath -> IO ()
serverUnix path =
  serveUnix path
    [ method "add"  add
    , method "echo" echo
    ]
  where
    add :: Int -> Int -> Server Int
    add x y = return $ x + y

    echo :: String -> Server String
    echo s = return $ "***" ++ s ++ "***"

clientUnix :: FilePath -> IO ()
clientUnix path = execClientUnix path $ do
  r1 <- add 123 456
  liftIO $ r1 @?= 123 + 456
  r2 <- echo "hello"
  liftIO $ r2 @?= "***hello***"
  where
    add :: Int -> Int -> Client Int
    add = call "add"

    echo :: String -> Client String
    echo = call "echo"

