module Language.MessagePack.IDL.Internal (
  withDirectory
  ) where

import Control.Exception
import System.Directory

withDirectory :: FilePath -> IO a -> IO a
withDirectory dir m = do
  createDirectoryIfMissing True dir
  bracket
    getCurrentDirectory
    setCurrentDirectory
    (\_ -> setCurrentDirectory dir >> m)
