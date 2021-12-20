{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Monad.Trans
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.MessagePack.Client
import           Network.MessagePack.Server
import           Network.Socket             (Socket, withSocketsDo)

import           System.IO                  (openTempFile)

port :: Int
port = 5000

main :: IO ()
main = do
  (f, _) <- openTempFile "/tmp" "socket.sock"
  withSocketsDo $ defaultMain $
    testGroup "simple service"
    [ testCase "test TCP"  $ testClientServer (clientTCP port) (serverTCP port)
    , testCase "test Unix" $ testClientServer (clientUnix f) (serverUnix f) ]

testClientServer :: IO () -> ((Socket -> IO ()) -> IO ()) -> IO ()
testClientServer client server = do
  (okChan :: Chan ()) <- newChan
  forkIO $ server (const $ writeChan okChan ())
  readChan okChan
  client

serverTCP :: Int -> (Socket -> IO ()) -> IO ()
serverTCP port afterBind =
  serve (setAfterBind afterBind $ serverSettings port "*")
    [ method "add"  add
    , method "echo" echo
    ]
  where
    add :: Int -> Int -> Server Int
    add x y = return $ x + y

    echo :: String -> Server String
    echo s = return $ "***" ++ s ++ "***"

clientTCP :: Int -> IO ()
clientTCP port = execClient (clientSettings port "localhost") $ do
  r1 <- add 123 456
  liftIO $ r1 @?= 123 + 456
  r2 <- echo "hello"
  liftIO $ r2 @?= "***hello***"
  where
    add :: Int -> Int -> Client Int
    add = call "add"

    echo :: String -> Client String
    echo = call "echo"

serverUnix :: FilePath -> (Socket -> IO ()) -> IO ()
serverUnix path afterBind =
  serveUnix (setAfterBind afterBind $ serverSettingsUnix path)
    [ method "add"  add
    , method "echo" echo
    ]
  where
    add :: Int -> Int -> Server Int
    add x y = return $ x + y

    echo :: String -> Server String
    echo s = return $ "***" ++ s ++ "***"

clientUnix :: FilePath -> IO ()
clientUnix path = execClientUnix (clientSettingsUnix path) $ do
  r1 <- add 123 456
  liftIO $ r1 @?= 123 + 456
  r2 <- echo "hello"
  liftIO $ r2 @?= "***hello***"
  where
    add :: Int -> Int -> Client Int
    add = call "add"

    echo :: String -> Client String
    echo = call "echo"

