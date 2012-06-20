{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.MessagePack.IDL.Check (
  check,
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Data
import Data.Lens.Lazy
import Data.Lens.Template
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Text.Shakespeare.Text

import Language.MessagePack.IDL.Syntax

data TcEnv
  = TcEnv
    { _envTypes :: M.Map T.Text Type
    }
makeLens ''TcEnv

emptyEnv :: TcEnv
emptyEnv = TcEnv
  { _envTypes = M.empty
  }

data TcError = TcError T.Text
  deriving (Show, Data, Typeable)

instance Error TcError where
  strMsg = TcError . T.pack

type TcM = StateT TcEnv (Either TcError)

check :: Spec -> Bool
check decls =
  let types = execStateT (mapM_ genTypes decls) emptyEnv
  in False

genTypes :: Decl -> TcM ()
genTypes decl = case decl of
  MPMessage {..} ->
    focus envTypes $ do
      mb <- access $ mapLens msgName
      when (isJust mb) $
        throwError $ TcError [st|message "#{msgName}" is already defined|]

  _ -> do
    return ()
