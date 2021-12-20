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
  serveUnix,

  -- * RPC server settings
  ServerSettings,
  serverSettings,
  U.ServerSettingsUnix,

  -- * Getters & setters
  SN.serverSettingsUnix,
  SN.getReadBufferSize,
  SN.setReadBufferSize,
  getAfterBind,
  setAfterBind,
  getPort,
  setPort,
  ) where

import           Conduit                           (MonadUnliftIO)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Control.Monad.Trans.Control
import           Data.Binary
import           Data.ByteString                   (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Binary               as CB
import           Data.Conduit.Network
import qualified Data.Conduit.Network.Unix         as U
import           Data.Conduit.Serialization.Binary
import           Data.List
import           Data.MessagePack
import           Data.MessagePack.Result
import qualified Data.Streaming.Network            as SN
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

-- | Start an RPC server with a set of RPC methods on a TCP socket.
serve :: (MonadBaseControl IO m, MonadUnliftIO m, MonadIO m, MonadCatch m, MonadThrow m)
         => ServerSettings -- ^ settings
         -> [Method m]     -- ^ list of methods
         -> m ()
serve settings methods = runGeneralTCPServer settings $ \ad -> do
  (rsrc, _) <- appSource ad $$+ return ()
  (_ :: Either ParseError ()) <- try $ processRequests methods rsrc (appSink ad)
  return ()

-- | Start an RPC server with a set of RPC methods on a Unix domain socket.
serveUnix :: (MonadBaseControl IO m, MonadIO m, MonadCatch m, MonadThrow m)
          => U.ServerSettingsUnix
          -> [Method m] -- ^ list of methods
          -> m ()
serveUnix settings methods = liftBaseWith $ \run ->
  U.runUnixServer settings $ \ad -> void . run $ do
    (rsrc, _) <- appSource ad $$+ return ()
    (_ :: Either ParseError ()) <- try $ processRequests methods rsrc (appSink ad)
    return ()

processRequests :: (MonadThrow m)
  => [Method m] -- ^ list of methods
  -> SealedConduitT () ByteString m ()
  -> ConduitT ByteString Void m a
  -> m b
processRequests methods rsrc sink = do
  (rsrc', res) <- rsrc $$++ do
    obj <- sinkGet get
    case fromObject obj of
      Error err  -> throwM $ ServerError $ "invalid request: " ++ err
      Success req -> lift $ getResponse (req :: Request)
  _ <- runConduit $ CB.sourceLbs (pack res) .| sink
  processRequests methods rsrc' sink
  where
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
