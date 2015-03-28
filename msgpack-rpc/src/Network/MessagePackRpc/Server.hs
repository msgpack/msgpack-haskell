{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Server
-- Copyright : (c) Hideyuki Tanaka, 2010-2015
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
-- > add :: Int -> Int -> Server IO Int
-- > add x y = return $ x + y
-- >
-- > main = serve 1234 [("add", toMethod add)]
--
--------------------------------------------------------------------

module Network.MessagePackRpc.Server (
  -- * RPC method types
  Method, MethodType(..),
  ServerT(..), Server,
  -- * Start RPC server
  serve,
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Control.Monad.Trans.Control
import           Data.Binary
import           Data.Conduit
import qualified Data.Conduit.Binary               as CB
import           Data.Conduit.Network
import           Data.Conduit.Serialization.Binary
import           Data.MessagePack
import           Data.Typeable

type Method m = [Object] -> m Object

type Request  = (Int, Int, String, [Object])
type Response = (Int, Int, Object, Object)

data ServerError = ServerError String
  deriving (Show, Typeable)

instance Exception ServerError

newtype ServerT m a = ServerT { runServerT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans ServerT where
  lift = ServerT

type Server = ServerT IO

class Monad m => MethodType m f where
  -- | Create a RPC method from a Hakell function
  toMethod :: f -> Method m

instance (MonadThrow m, MessagePack o) => MethodType m (ServerT m o) where
  toMethod m ls = case ls of
    [] -> toObject <$> runServerT m
    _  -> throwM $ ServerError "argument number error"

instance (MonadThrow m, MessagePack o, MethodType m r) => MethodType m (o -> r) where
  toMethod f (x: xs) =
    case fromObject x of
      Nothing -> throwM $ ServerError "argument type error"
      Just r  -> toMethod (f r) xs

-- | Start RPC server with a set of RPC methods.
serve :: (MonadBaseControl IO m, MonadIO m, MonadCatch m, MonadThrow m)
         => Int                     -- ^ Port number
         -> [(String, Method m)] -- ^ list of (method name, RPC method)
         -> m ()
serve port methods = runGeneralTCPServer (serverSettings port "*") $ \ad -> do
  (rsrc, _) <- appSource ad $$+ return ()
  (_ :: Either ParseError ()) <- try $ processRequests rsrc (appSink ad)
  return ()
  where
    processRequests rsrc sink = do
      (rsrc', res) <- rsrc $$++ do
        obj <- sinkGet get
        case fromObject obj of
          Nothing  -> throwM $ ServerError "invalid request"
          Just req -> lift $ getResponse (req :: Request)
      _ <- CB.sourceLbs (pack res) $$ sink
      processRequests rsrc' sink

    getResponse (rtype, msgid, methodName, args) = do
      when (rtype /= 0) $
        throwM $ ServerError $ "request type is not 0, got " ++ show rtype
      ret <- callMethod methodName args
      return ((1, msgid, toObject (), ret) :: Response)

    callMethod methodName args =
      case lookup methodName methods of
        Nothing ->
          throwM $ ServerError $ "method '" ++ methodName ++ "' not found"
        Just method ->
          method args
