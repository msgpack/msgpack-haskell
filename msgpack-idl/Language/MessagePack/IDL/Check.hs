{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
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
import Data.List
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

check :: Spec -> Either TcError ()
check decls = evalStateT (check' decls) emptyEnv

check' :: Spec -> TcM ()
check' decls = do
  mapM_ getTypes decls
  mapM_ typeCheck decls

getTypes :: Decl -> TcM ()
getTypes decl = case decl of
  MPMessage {..} ->
    addType msgName undefined

  MPException {..} ->
    addType excName undefined

  MPType {..} ->
    addType tyName tyType

  MPEnum {..} ->
    undefined

  MPService {..} ->
    return ()

addType :: T.Text -> Type -> TcM ()
addType name typ = do
  focus envTypes $ do
    mb <- access $ mapLens name
    when (isJust mb) $
      throwError $ TcError [st|message "#{name}" is already defined|]
    mapLens name ~= Just typ
    return ()

typeCheck :: Decl -> TcM ()
typeCheck decl = case decl of
  MPMessage {..} -> do
    -- Check each field
    mapM_ (checkField msgParam) msgFields
    -- Are ids unique?
    checkIf "field ids are not unique" (checkUnique $ map fldId msgFields)
    -- Are names unique?
    checkIf "field names are not unique" (checkUnique $ map fldName msgFields)
    -- TODO: check type of literal
    return ()

  MPException {..} -> do
    -- Check each field
    mapM_ (checkField excParam) excFields
    -- Are ids unique?
    checkIf "field ids are not unique" (checkUnique $ map fldId excFields)
    -- Are names unique?
    checkIf "field names are not unique" (checkUnique $ map fldName excFields)
    -- TODO: check type of literal
    -- TODO: check super type is also an exception, and do not loop.
    return ()

  MPType {..} ->
    return ()

  MPEnum {..} ->
    undefined

  MPService {..} -> do
    -- TODO:
    undefined

checkUnique :: Eq a => [a] -> Bool
checkUnique ls = length ls == length (nub ls)

checkField :: [T.Text] -> Field -> TcM ()
checkField _ _ = return ()

checkIf :: T.Text -> Bool -> TcM ()
checkIf msg b =
  when (not b) $ throwError $ TcError msg
