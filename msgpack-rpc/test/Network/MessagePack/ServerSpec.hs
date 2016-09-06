{-# LANGUAGE OverloadedStrings #-}
module Network.MessagePack.ServerSpec where

import           Test.Hspec

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Trans
import           Network                    (withSocketsDo)

import           Network.MessagePack.Client
import           Network.MessagePack.Server

port :: Int
port = 5000


spec :: Spec
spec =
  describe "simple service" $
    it "test" $ withSocketsDo $
      server `race_` (threadDelay 1000 >> client)


server :: IO ()
server =
  serve port
    [ method "add"  add
    , method "echo" echo
    ]
  where
    add :: Int -> Int -> Server Int
    add x y = return $ x + y

    echo :: String -> Server String
    echo s = return $ "***" ++ s ++ "***"


client :: IO ()
client = execClient "127.0.0.1" port $ do
  r1 <- add 123 456
  liftIO $ r1 `shouldBe` 123 + 456
  r2 <- echo "hello"
  liftIO $ r2 `shouldBe` "***hello***"
  where
    add :: Int -> Int -> Client Int
    add = call "add"

    echo :: String -> Client String
    echo = call "echo"
