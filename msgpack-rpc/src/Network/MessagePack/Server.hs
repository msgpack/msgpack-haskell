{-# LANGUAGE DeriveDataTypeable         #-}
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
-- > import Network.MessagePack.Server
-- >
-- > add :: Int -> Int -> Server Int
-- > add x y = return $ x + y
-- >
-- > main = serve 1234 [ method "add" add ]
--
--------------------------------------------------------------------

module Network.MessagePack.Server (
  -- * RPC method types
  Method, MethodType(..),
  ServerT(..), Server,
  -- * Build a method
  method,
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
import           Data.List
import           Data.MessagePack
import           Data.Typeable

-- ^ MessagePack RPC method
data Method m
  = Method
    { methodName :: String
    , methodBody :: [Object] -> m Object
    }

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
  toBody :: f -> [Object] -> m Object

instance (Functor m, MonadThrow m, MessagePack o) => MethodType m (ServerT m o) where
  toBody m ls = case ls of
    [] -> toObject <$> runServerT m
    _  -> throwM $ ServerError "argument number error"

instance (MonadThrow m, MessagePack o, MethodType m r) => MethodType m (o -> r) where
  toBody f (x: xs) =
    case fromObject x of
      Error e   -> throwM $ ServerError e
      Success r -> toBody (f r) xs

-- | Build a method
method :: MethodType m f
          => String   -- ^ Method name
          -> f        -- ^ Method body
          -> Method m
method name body = Method name $ toBody body

-- | Start RPC server with a set of RPC methods.
serve :: (MonadBaseControl IO m, MonadIO m, MonadCatch m, MonadThrow m)
         => Int        -- ^ Port number
         -> [Method m] -- ^ list of methods
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
          Error e     -> throwM $ ServerError e
          Success req -> lift $ getResponse (req :: Request)
      _ <- CB.sourceLbs (pack res) $$ sink
      processRequests rsrc' sink

    getResponse (rtype, msgid, methodName, args) = do
      when (rtype /= 0) $
        throwM $ ServerError $ "request type is not 0, got " ++ show rtype
      ret <- callMethod methodName args
      return ((1, msgid, toObject (), ret) :: Response)

    callMethod name args =
      case find ((== name) . methodName) methods of
        Nothing ->
          throwM $ ServerError $ "method '" ++ name ++ "' not found"
        Just m ->
          methodBody m args
