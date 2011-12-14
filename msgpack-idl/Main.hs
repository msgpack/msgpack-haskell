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

import Paths_msgpack_idl

data MPIDL
  = Haskell
  | Cpp
    { output_dir :: FilePath
    , namespace :: String
    , pficommon :: Bool
    , filepath :: FilePath }
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
          ]
    &= help "MessagePack RPC IDL Compiler"
    &= summary ("mpidl " ++ showVersion version)

  print conf
  compile conf
  
compile :: MPIDL -> IO ()
compile Cpp {..} = do
  espec <- parseFile idl filepath
  case espec of
    Left err -> do
      print err
    Right spec -> do
      print spec
      withDirectory output_dir $ do
        Cpp.generate (Cpp.Config filepath namespace pficommon) spec

withDirectory :: FilePath -> IO a -> IO a
withDirectory dir m = do
  createDirectoryIfMissing True dir
  bracket
    getCurrentDirectory
    setCurrentDirectory
    (\_ -> setCurrentDirectory dir >> m)
