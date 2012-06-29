module Language.MessagePack.IDL.Desugar (
  desugar,
  ) where

import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Text as T

import Language.MessagePack.IDL.Syntax

type DsM = StateT () Identity

type CoreSpec = [CoreDecl]

data CoreDecl
  = CMessage
    { cmsgName :: T.Text
    , cmsgParam :: [T.Text]
    , cmsgFields :: [CoreField]
    , cmsgIsException :: Maybe T.Text
    }
  | CType
    { ctyName :: T.Text
    , ctyType :: CoreType
    }

  | CService
    { cserviceName :: T.Text
    , cserviceVersion :: Int
    , serviceMethods :: [CoreMethod]
    }

data CoreMethod
  = CMethod
    { cmethodName :: T.Text
    , cmethodVersion :: Int
    , cmethodRetType :: CoreType
    , cmethodArgs :: [CoreField]
    }

data CoreType
  = CInt Bool Int
  | CFloat Bool
  | CBool
  | CRaw
  | CString
  | CObject
  | CVoid

  | CNullable CoreType
  | CList CoreType
  | CMap CoreType
  | CTyple [CoreType]
  | CUserDef T.Text [CoreType] [CoreField]

data CoreField = CoreField

desugar :: Spec -> DsM CoreSpec
desugar = undefined
