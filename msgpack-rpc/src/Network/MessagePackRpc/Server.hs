{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, Rank2Types          #-}

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
-- > add :: Int -> Int -> Method Int
-- > add x y = return $ x + y
-- >
-- > main = serve 1234 [("add", toMethod add)]
--
--------------------------------------------------------------------

module Network.MessagePackRpc.Server (
  -- * RPC method types
  RpcMethod, MethodType(..),
  Method(..),
  -- * Start RPC server
  serve,
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Data.Binary
import           Data.Conduit
import qualified Data.Conduit.Binary               as CB
import           Data.Conduit.Network
import           Data.Conduit.Serialization.Binary
import           Data.MessagePack
import           Data.Typeable

type RpcMethod = [Object] -> IO Object

type Request  = (Int, Int, String, [Object])
type Response = (Int, Int, Object, Object)

data ServerError = ServerError String
  deriving (Show, Typeable)

instance Exception ServerError

newtype Method a = Method { runMethod :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

class MethodType f where
  -- | Create a RPC method from a Hakell function
  toMethod :: f -> RpcMethod

instance MessagePack o => MethodType (Method o) where
  toMethod m ls = case ls of
    [] -> toObject <$> runMethod m
    _  -> throwM $ ServerError "argument number error"

instance (MessagePack o, MethodType r) => MethodType (o -> r) where
  toMethod f (x: xs) =
    case fromObject x of
      Nothing -> throwM $ ServerError "argument type error"
      Just r  -> toMethod (f r) xs

-- | Start RPC server with a set of RPC methods.
serve :: Int                        -- ^ Port number
         -> [(String, RpcMethod)] -- ^ list of (method name, RPC method)
         -> IO ()
serve port methods = runTCPServer (serverSettings port "*") $ \ad -> do
  (rsrc, _) <- appSource ad $$+ return ()
  _ <- try $ processRequests rsrc (appSink ad) :: IO (Either ParseError ())
  return ()
  where
    processRequests rsrc sink = do
      (rsrc', res) <- rsrc $$++ do
        obj <- sinkGet get
        case fromObject obj of
          Nothing  -> throwM $ ServerError "invalid request"
          Just req -> lift $ getResponse req
      _ <- CB.sourceLbs (pack res) $$ sink
      processRequests rsrc' sink

    getResponse :: Request -> IO Response
    getResponse (rtype, msgid, methodName, args) = do
      when (rtype /= 0) $
        throwM $ ServerError $ "request type is not 0, got " ++ show rtype
      ret <- callMethod methodName args
      return (1, msgid, toObject (), ret)

    callMethod :: String -> [Object] -> IO Object
    callMethod methodName args =
      case lookup methodName methods of
        Nothing ->
          throwM $ ServerError $ "method '" ++ methodName ++ "' not found"
        Just method ->
          method args
