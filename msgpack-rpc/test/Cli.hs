{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- import Network.MessagePackRpc.Client

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Control
import Control.Monad.State
import System.IO

import Data.MessagePack

newtype TestT m a
  = TestT { unTestT :: StateT () m a }
  deriving (Monad, MonadIO, MonadControlIO, MonadTrans, MonadState ())

class RpcMethod r where
  foo :: r

instance (MonadControlIO m, OBJECT a) => RpcMethod (TestT m a) where
  foo = undefined

instance (RpcMethod m, OBJECT a) => RpcMethod (a -> m) where
  foo = undefined
  
method :: RpcMethod m => String -> m
method methodName = undefined

newtype Connection
  = Connection { unConnection :: MVar Handle }

doRPC :: Connection -> String -> [Object] -> IO Object
doRPC conn m args = withMVar (unConnection conn) $ \h -> do
  msgid <- (`mod`2^(30::Int)) <$> randomIO :: IO Int
  BL.hPutStr h $ pack (0 ::Int, msgid, m, args)
  hFlush h
  run_ $ enumHandle bufferSize h $$ do
    (rtype, rmsgid, rerror, rresult) <- iterParser get
    when (rtype /= (1 :: Int)) $
      throw $ ProtocolError $ "response type is not 1 (got " ++ show rtype ++ ")"
    when (rmsgid /= msgid) $
      throw $ ProtocolError $ "message id mismatch: expect " ++ show msgid ++ ", but got " ++ show rmsgid
    case tryFromObject rerror of
      Left _ ->
        throw $ ServerError rerror
      Right () ->
        return rresult

--

runTestT :: MonadControlIO m => String -> Int -> TestT m a -> m a
runTestT = undefined

--

add :: MonadControlIO m => Int -> Int -> TestT m Int
add  = method "add"

echo :: MonadControlIO m => String -> TestT m String
echo = method "echo"

main :: IO ()
main = do
  runTestT "localhost" 8081 $ do
    liftIO . print =<< add 123 456
    liftIO . print =<< echo "Hello"
