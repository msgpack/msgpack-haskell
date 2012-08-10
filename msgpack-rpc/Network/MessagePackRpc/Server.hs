{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Server
-- Copyright : (c) Hideyuki Tanaka, 2010-2012
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
-- > add :: Int -> Int -> Method Int
-- > add x y = return $ x + y
-- >
-- > main = serve 1234 [("add", toMethod add)]
--
--------------------------------------------------------------------

module Network.MessagePackRpc.Server (
  -- * RPC method types
  RpcMethod, MethodType(..),
  MethodT(..), Method,
  -- * Start RPC server
  serve,
  ) where

import Control.Applicative
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Conduit
import Data.Conduit.Network
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Attoparsec as CA
import Data.Data
import Data.MessagePack

type RpcMethod m = [Object] -> m Object

type Request  = (Int, Int, String, [Object])
type Response = (Int, Int, Object, Object)

data ServerError = ServerError String
  deriving (Show, Typeable)

instance Exception ServerError

type Method = MethodT IO

newtype MethodT m a = MethodT { unMethodT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans MethodT where
  lift = MethodT

class MethodType f m | f -> m where
  -- | Create a RPC method from a Hakell function
  toMethod :: f -> RpcMethod m

instance (MonadThrow m, MonadBaseControl IO m, OBJECT o)
         => MethodType (MethodT m o) m where
  toMethod m ls = case ls of
    [] -> toObject <$> unMethodT m
    _ -> monadThrow $ ServerError "argument error"

instance (OBJECT o, MethodType r m) => MethodType (o -> r) m where
  toMethod f = \(x:xs) -> toMethod (f $! fromObject' x) xs

fromObject' :: OBJECT o => Object -> o
fromObject' o =
  case tryFromObject o of
    Left err -> error $ "argument type error: " ++ err
    Right r -> r

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
      _ <- CB.sourceLbs (pack res) $$ sink
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
