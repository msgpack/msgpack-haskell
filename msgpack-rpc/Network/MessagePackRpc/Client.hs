{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Client
-- Copyright : (c) Hideyuki Tanaka, 2010-2012
-- License   : BSD3
--
-- Maintainer:  Hideyuki Tanaka <tanaka.hideyuki@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- This module is client library of MessagePack-RPC.
-- The specification of MessagePack-RPC is at
-- <http://redmine.msgpack.org/projects/msgpack/wiki/RPCProtocolSpec>.
--
-- A simple example:
--
-- >import Network.MessagePackRpc.Client
-- >
-- >add :: RpcMethod (Int -> Int -> IO Int)
-- >add = method "add"
-- >
-- >main = do
-- >  conn <- connect "127.0.0.1" 1234
-- >  print =<< add conn 123 456
--
--------------------------------------------------------------------

module Network.MessagePackRpc.Client (
  -- * MPRPC type
  MPRPCT, MPRPC,
  runMPRPCClient,

  -- * Call RPC method
  call,

  -- * RPC error
  RpcError(..),
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.State as CMS
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Network
import Data.Functor
import Data.MessagePack as M
import Data.Typeable
import Network
import System.IO
import System.Random

type MPRPC = MPRPCT IO

newtype MPRPCT m a
  = MPRPCT { unMPRPCT :: StateT (Connection m) m a }
  deriving (Monad, MonadIO)

-- | RPC connection type
data Connection m
  = Connection
    { connSource :: !(ResumableSource m S.ByteString)
    , connSink   :: !(Sink S.ByteString m ())
    , connCnt    :: !Int
    }

runMPRPCClient :: (MonadIO m, MonadBaseControl IO m)
                  => String -> Int -> MPRPCT m a -> m ()
runMPRPCClient host port m = do
  runTCPClient (ClientSettings port host) $ \src sink -> do
    (rsrc, _) <- src $$+ return ()
    evalStateT (unMPRPCT m) (Connection rsrc sink 0)
    return ()

-- | RPC error type
data RpcError
  = ServerError Object     -- ^ Server error
  | ResultTypeError String -- ^ Result type mismatch
  | ProtocolError String   -- ^ Protocol error
  deriving (Show, Eq, Ord, Typeable)

instance Exception RpcError

class RpcType r where
  rpcc :: String -> [Object] -> r

fromObject' :: OBJECT o => Object -> o
fromObject' o =
  case tryFromObject o of
    Left err -> throw $ ResultTypeError err
    Right r -> r

instance (MonadIO m, MonadThrow m, OBJECT o) => RpcType (MPRPCT m o) where
  rpcc m args = return . fromObject' =<< rpcCall m (reverse args)

instance (OBJECT o, RpcType r) => RpcType (o -> r) where
  rpcc m args arg = rpcc m (toObject arg:args)

rpcCall :: (MonadIO m, MonadThrow m) => String -> [Object] -> MPRPCT m Object
rpcCall methodName args = MPRPCT $ do
  Connection rsrc sink msgid <- CMS.get
  lift $ CB.sourceLbs (pack (0 :: Int, msgid, methodName, args)) $$ sink
  (rsrc', ret) <- lift $ rsrc $$++ do
    (rtype, rmsgid, rerror, rresult) <- CA.sinkParser M.get
    when (rtype /= (1 :: Int)) $
      throw $ ProtocolError $
        "invalid response type (expect 1, but got " ++ show rtype ++ ")"
    when (rmsgid /= msgid) $
      throw $ ProtocolError $
        "message id mismatch: expect "
        ++ show msgid ++ ", but got "
        ++ show rmsgid
    case tryFromObject rerror of
      Left _ ->
        throw $ ServerError rerror
      Right () ->
        return rresult
  CMS.put $ Connection rsrc' sink (msgid + 1)
  return ret

{-
rpcCall :: Connection -> String -> [Object] -> IO Object
rpcCall Connection{ connHandle = mh } m args = withMVar mh $ \h -> do
  msgid <- (`mod`2^(30::Int)) <$> randomIO :: IO Int
  BL.hPutStr h $ pack (0 ::Int, msgid, m, args)
  hFlush h
  C.runResourceT $ CB.sourceHandle h C.$$ do
    (rtype, rmsgid, rerror, rresult) <- CA.sinkParser get
    when (rtype /= (1 :: Int)) $
      throw $ ProtocolError $ "response type is not 1 (got " ++ show rtype ++ ")"
    when (rmsgid /= msgid) $
      throw $ ProtocolError $ "message id mismatch: expect " ++ show msgid ++ ", but got " ++ show rmsgid
    case tryFromObject rerror of
      Left _ ->
        throw $ ServerError rerror
      Right () ->
        return rresult
-}

-- | Call an RPC Method
call :: RpcType a
        => String -- ^ Method name
        -> a
call m = rpcc m []
