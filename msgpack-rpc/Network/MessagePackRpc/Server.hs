{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies, KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Server
-- Copyright : (c) Hideyuki Tanaka, 2010
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- This module is server library of MessagePack-RPC.
-- The specification of MessagePack-RPC is at
-- <http://redmine.msgpack.org/projects/msgpack/wiki/RPCProtocolSpec>.
--
-- A simple example:
--
-- > import Network.MessagePackRpc.Server
-- >
-- > add :: Int -> Int -> IO Int
-- > add x y = return $ x + y
-- >
-- > main =
-- >   serve 1234 [("add", fun add)]
--
--------------------------------------------------------------------

module Network.MessagePackRpc.Server (
  -- * RPC method types
  RpcMethod,
  RpcMethodType(..),
  MethodT(..),
  -- * Create RPC method
  fun,
  -- * Start RPC server
  serve,
  ) where

import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Conduit
import Data.Conduit.Network
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Attoparsec as CA
import Data.Data
import Data.Maybe
import Data.MessagePack
import System.IO

import Prelude hiding (catch)

type RpcMethod m = [Object] -> m Object

type Request  = (Int, Int, String, [Object])
type Response = (Int, Int, Object, Object)

data ServerError = ServerError String
  deriving (Show, Typeable)

instance Exception ServerError

newtype MethodT m a = MethodT { unMethodT :: m a }

class RpcMethodType f where
  type BaseM f :: * -> *
  toRpcMethod :: f -> RpcMethod (BaseM f)

instance (MonadThrow m, MonadBaseControl IO m, OBJECT o)
         => RpcMethodType (MethodT m o) where
  type BaseM (MethodT m o) = m
  toRpcMethod m ls = case ls of
    [] -> toObject <$> unMethodT m
    _ -> monadThrow $ ServerError "argument error"

instance (OBJECT o, RpcMethodType r) => RpcMethodType (o -> r) where
  type BaseM (o -> r) = BaseM r
  toRpcMethod f = \(x:xs) -> toRpcMethod (f $! fromObject' x) xs

fromObject' :: OBJECT o => Object -> o
fromObject' o =
  case tryFromObject o of
    Left err -> error $ "argument type error: " ++ err
    Right r -> r

-- | Create a RPC method from a Haskell function.
fun :: RpcMethodType f => f -> RpcMethod (BaseM f)
fun = toRpcMethod

-- | Start RPC server with a set of RPC methods.
serve :: forall m . (MonadIO m, MonadThrow m, MonadBaseControl IO m)
         => Int                     -- ^ Port number
         -> [(String, RpcMethod m)] -- ^ list of (method name, RPC method)
         -> m ()
serve port methods = runTCPServer (ServerSettings port "*") $ \src sink -> do
  (rsrc, _) <- src $$+ return ()
  processRequests rsrc sink
  where
    processRequests rsrc sink = do
      (rsrc', res) <- rsrc $$++ do
        req <- CA.sinkParser get
        lift $ getResponse req
      CB.sourceLbs (pack res) $$ sink
      processRequests rsrc' sink

    getResponse :: Request -> m Response
    getResponse (rtype, msgid, methodName, args) = do
      when (rtype /= 0) $
        monadThrow $ ServerError $ "request type is not 0, got " ++ show rtype
      ret <- callMethod methodName args
      return (1, msgid, toObject (), ret)

    callMethod :: String -> [Object] -> m Object
    callMethod methodName args =
      case lookup methodName methods of
        Nothing ->
          monadThrow $ ServerError $ "method '" ++ methodName ++ "' not found"
        Just method ->
          method args

{-
    processRequest h = do
      (rtype, msgid, method, args) <- CA.sinkParser get
      liftIO $ do
        resp <- try $ getResponse rtype method args
        case resp of
          Left err ->
            BL.hPutStr h $ pack (1 :: Int, msgid :: Int, show (err :: SomeException), ())
          Right ret ->
            BL.hPutStr h $ pack (1 :: Int, msgid :: Int, (), ret)
        hFlush h

    getResponse rtype method args = do
      when (rtype /= (0 :: Int)) $
        fail "request type is not 0"

      r <- callMethod (method :: String) (args :: [Object])
      r `deepseq` return r

    callMethod methodName args =
      case lookup methodName methods of
        Nothing ->
          fail $ "method '" ++ methodName ++ "' not found"
        Just method ->
          method args
-}
