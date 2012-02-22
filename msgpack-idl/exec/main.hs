{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Version
import System.Console.CmdArgs
import Text.Peggy

import Language.MessagePack.IDL
import Language.MessagePack.IDL.Internal
import qualified Language.MessagePack.IDL.CodeGen.Haskell as Haskell
import qualified Language.MessagePack.IDL.CodeGen.Cpp as Cpp
import qualified Language.MessagePack.IDL.CodeGen.Ruby as Ruby
import qualified Language.MessagePack.IDL.CodeGen.Java as Java
import qualified Language.MessagePack.IDL.CodeGen.Php as Php
import qualified Language.MessagePack.IDL.CodeGen.Python as Python
import qualified Language.MessagePack.IDL.CodeGen.Perl as Perl
import qualified Language.MessagePack.IDL.CodeGen.Erlang as Erlang

import Paths_msgpack_idl

data MPIDL
  = Haskell
    { output_dir :: FilePath
    , module_name :: String
    , filepath :: FilePath
    }
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
  | Python
    { output_dir :: FilePath
    , filepath :: FilePath
    }
  | Perl
    { output_dir :: FilePath
    , namespace :: String
    , filepath :: FilePath }
  | Erlang
    { output_dir :: FilePath
    , filepath :: FilePath }
  deriving (Show, Eq, Data, Typeable)

main :: IO ()
main = do
  conf <- cmdArgs $
    modes [ Haskell
            { output_dir = def
            , module_name = ""
            , filepath = def &= argPos 0
            }
          , Cpp
            { output_dir = def
            , namespace = "msgpack"
            , pficommon = False
            , filepath = def &= argPos 0
            }
          , Ruby
            { output_dir = def
            , modules = "MessagePack"
            , filepath = def &= argPos 0
            }
          , Java
            { output_dir = def
            , package = "msgpack"
            , filepath = def &= argPos 0
            }
          , Php
            { output_dir = def
            , filepath = def &= argPos 0
            }
          , Python
            { output_dir = def
            , filepath = def &= argPos 0
            }
          , Perl
            { output_dir = def
            , namespace = "msgpack"
            , filepath = def &= argPos 0
            }
          , Erlang
            { output_dir = def
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
      withDirectory (output_dir conf) $ do
        case conf of
          Cpp {..} -> do
            Cpp.generate (Cpp.Config filepath namespace pficommon) spec
          
          Haskell {..} -> do
            Haskell.generate (Haskell.Config filepath) spec
        
          Java {..} -> do
            Java.generate (Java.Config filepath package) spec

          Perl {..} -> do
            Perl.generate (Perl.Config filepath namespace) spec

          Php {..} -> do
            Php.generate (Php.Config filepath) spec
 
          Python {..} -> do
            Python.generate (Python.Config filepath) spec
 
          Ruby {..} -> do
            Ruby.generate (Ruby.Config filepath modules) spec
          
          Erlang {..} -> do
            Erlang.generate (Erlang.Config filepath) spec

