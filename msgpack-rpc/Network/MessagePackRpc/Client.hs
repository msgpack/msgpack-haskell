{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}

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
-- > import Network.MessagePackRpc.Client
-- >
-- > add :: Int -> Int -> MPRPC Int
-- > add = call "add"
-- >
-- > main = runMPRPC "localhost" 5000 $ do
-- >   liftIO . print =<< add 123 456
--
--------------------------------------------------------------------

module Network.MessagePackRpc.Client (
  -- * MPRPC type
  MPRPCT, MPRPC,
  runClient,

  -- * Call RPC method
  call,

  -- * RPC error
  RpcError(..),
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.State.Strict as CMS
import qualified Data.ByteString as S
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Network
import Data.MessagePack as M
import Data.Typeable

type MPRPC = MPRPCT IO

newtype MPRPCT m a
  = MPRPCT { unMPRPCT :: StateT (Connection m) m a }
  deriving (Monad, MonadIO, MonadThrow)

-- It can't derive by newtype deriving...
instance MonadTrans MPRPCT where
  lift = MPRPCT . lift

-- | RPC connection type
data Connection m where
  Connection ::
    !(ResumableSource m S.ByteString)
    -> !(Sink S.ByteString m ())
    -> !Int
    -> Connection m

runClient :: (MonadIO m, MonadBaseControl IO m)
             => String -> Int -> MPRPCT m a -> m ()
runClient host port m = do
  runTCPClient (ClientSettings port host) $ \src sink -> do
    (rsrc, _) <- src $$+ return ()
    void $ evalStateT (unMPRPCT m) (Connection rsrc sink 0)

-- | RPC error type
data RpcError
  = ServerError Object     -- ^ Server error
  | ResultTypeError String -- ^ Result type mismatch
  | ProtocolError String   -- ^ Protocol error
  deriving (Show, Eq, Ord, Typeable)

instance Exception RpcError

class RpcType r where
  rpcc :: String -> [Object] -> r

instance (MonadIO m, MonadThrow m, OBJECT o) => RpcType (MPRPCT m o) where
  rpcc m args = do
    res <- rpcCall m (reverse args)
    case tryFromObject res of
      Left err -> monadThrow $ ResultTypeError err
      Right r  -> return r

instance (OBJECT o, RpcType r) => RpcType (o -> r) where
  rpcc m args arg = rpcc m (toObject arg:args)

rpcCall :: (MonadIO m, MonadThrow m) => String -> [Object] -> MPRPCT m Object
rpcCall methodName args = MPRPCT $ do
  Connection rsrc sink msgid <- CMS.get
  (rsrc', (rtype, rmsgid, rerror, rresult)) <- lift $ do
    CB.sourceLbs (pack (0 :: Int, msgid, methodName, args)) $$ sink
    rsrc $$++ CA.sinkParser M.get
  CMS.put $ Connection rsrc' sink (msgid + 1)

  when (rtype /= (1 :: Int)) $
    monadThrow $ ProtocolError $
      "invalid response type (expect 1, but got " ++ show rtype ++ ")"
  when (rmsgid /= msgid) $
    monadThrow $ ProtocolError $
      "message id mismatch: expect "
      ++ show msgid ++ ", but got "
      ++ show rmsgid
  case tryFromObject rerror of
    Left _ ->
      monadThrow $ ServerError rerror
    Right () ->
      return rresult

-- | Call an RPC Method
call :: RpcType a
        => String -- ^ Method name
        -> a
call m = rpcc m []
