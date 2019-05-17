{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Client
-- Copyright : (c) Hideyuki Tanaka, 2010-2015
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
-- > import Network.MessagePack.Client
-- >
-- > add :: Int -> Int -> Client Int
-- > add = call "add"
-- >
-- > main = execClient "localhost" 5000 $ do
-- >   ret <- add 123 456
-- >   liftIO $ print ret
--
--------------------------------------------------------------------

module Network.MessagePack.Client (
  -- * MessagePack Client type
  Client, execClient,

  -- * Call RPC method
  call,

  -- * RPC error
  RpcError(..),
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.State.Strict        as CMS
import           Data.Binary                       as Binary
import qualified Data.ByteString                   as S
import           Data.Conduit
import qualified Data.Conduit.Binary               as CB
import           Data.Conduit.Network
import           Data.Conduit.Serialization.Binary
import           Data.MessagePack
import           Data.Typeable
import           System.IO

newtype Client a
  = ClientT { runClient :: StateT Connection IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

-- | RPC connection type
data Connection
  = Connection
    !(ResumableSource IO S.ByteString)
    !(Sink S.ByteString IO ())
    !Int

execClient :: S.ByteString -> Int -> Client a -> IO ()
execClient host port m =
  runTCPClient (clientSettings port host) $ \ad -> do
    (rsrc, _) <- appSource ad $$+ return ()
    void $ evalStateT (runClient m) (Connection rsrc (appSink ad) 0)

-- | RPC error type
data RpcError
  = ServerError Object     -- ^ Server error
  | ResultTypeError String -- ^ Result type mismatch
  | ProtocolError String   -- ^ Protocol error
  deriving (Show, Eq, Ord, Typeable)

instance Exception RpcError

class RpcType r where
  rpcc :: String -> [Object] -> r

instance MessagePack o => RpcType (Client o) where
  rpcc m args = do
    res <- rpcCall m (reverse args)
    case fromObject res of
      Success r -> return r
      Error e   -> throwM $ ResultTypeError e

instance (MessagePack o, RpcType r) => RpcType (o -> r) where
  rpcc m args arg = rpcc m (toObject arg:args)

rpcCall :: String -> [Object] -> Client Object
rpcCall methodName args = ClientT $ do
  Connection rsrc sink msgid <- CMS.get
  (rsrc', res) <- lift $ do
    CB.sourceLbs (pack (0 :: Int, msgid, methodName, args)) $$ sink
    rsrc $$++ sinkGet Binary.get
  CMS.put $ Connection rsrc' sink (msgid + 1)

  case fromObject res of
    Error e -> throwM $ ProtocolError e
    Success (rtype, rmsgid, rerror, rresult) -> do

      when (rtype /= (1 :: Int)) $
        throwM $ ProtocolError $
          "invalid response type (expect 1, but got " ++ show rtype ++ ")"

      when (rmsgid /= msgid) $
        throwM $ ProtocolError $
          "message id mismatch: expect "
          ++ show msgid ++ ", but got "
          ++ show rmsgid

      case fromObject rerror of
        Error e    -> throwM $ ServerError rerror
        Success () -> return rresult

-- | Call an RPC Method
call :: RpcType a
        => String -- ^ Method name
        -> a
call m = rpcc m []
