{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import Blaze.ByteString.Builder
import Control.Exception
import qualified Data.ByteString as B
import Data.Version
import System.Console.CmdArgs
import System.Directory
import Text.Peggy

import Language.MessagePack.IDL
import qualified Language.MessagePack.IDL.CodeGen.Haskell as Haskell
import qualified Language.MessagePack.IDL.CodeGen.Cpp as Cpp
import qualified Language.MessagePack.IDL.CodeGen.Ruby as Ruby
import qualified Language.MessagePack.IDL.CodeGen.Java as Java
import qualified Language.MessagePack.IDL.CodeGen.Php as Php

import Paths_msgpack_idl

data MPIDL
  = Haskell
  | Cpp
    { output_dir :: FilePath
    , namespace :: String
    , pficommon :: Bool
    , filepath :: FilePath }
  | Ruby
    { output_dir :: FilePath
    , modules :: String
    , filepath :: FilePath }
  | Java
    { output_dir :: FilePath
    , package :: String
    , filepath :: FilePath
    }
  | Php
    { output_dir :: FilePath
    , filepath :: FilePath
    }
  deriving (Show, Eq, Data, Typeable)

main :: IO ()
main = do
  conf <- cmdArgs $
    modes [ Haskell
          , Cpp { output_dir = def
                , namespace = "msgpack"
                , pficommon = False
                , filepath = def &= argPos 0
                }
          , Ruby { output_dir = def
                 , modules = "MessagePack"
                 , filepath = def &= argPos 0
                 }
          , Java { output_dir = def
                 , package = "msgpack"
                 , filepath = def &= argPos 0
                 }
          , Php { output_dir = def
                , filepath = def &= argPos 0
                }
          ]
    &= help "MessagePack RPC IDL Compiler"
    &= summary ("mpidl " ++ showVersion version)

  compile conf

compile :: MPIDL -> IO ()
compile conf = do
  espec <- parseFile idl (filepath conf)
  case espec of
    Left err -> do
      print err
    Right spec -> do
      print spec
      case conf of
        Cpp {..} -> do
          withDirectory output_dir $ do
            Cpp.generate (Cpp.Config filepath namespace pficommon) spec
        
        Java {..} -> do
          withDirectory (output_dir ++ "/" ++ package) $ do
            Java.generate (Java.Config filepath package) spec

        Php {..} -> do
          withDirectory (output_dir) $ do
            Php.generate (Php.Config filepath) spec

compile Ruby {..} = do
  espec <- parseFile idl filepath
  case espec of
    Left err -> do
      print err
    Right spec -> do
      print spec
      withDirectory output_dir $ do
        Ruby.generate (Ruby.Config filepath modules) spec

withDirectory :: FilePath -> IO a -> IO a
withDirectory dir m = do
  createDirectoryIfMissing True dir
  bracket
    getCurrentDirectory
    setCurrentDirectory
    (\_ -> setCurrentDirectory dir >> m)
