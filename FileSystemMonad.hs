{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileSystemMonad (
    runCommandLocally,
    FileSystemMonad(..),
    RealFileSystem(..)
) where

import System.IO (hGetContents)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import System.Process

-- Function that allows strings to be run as commands in the current shell context
runCommandLocally :: String -> IO String
runCommandLocally cmd = do
    (_, Just hout, _, _) <- createProcess (shell cmd) { std_out = CreatePipe }
    output <- hGetContents hout
    return output

-- Define the FileSystemMonad type class
class Monad m => FileSystemMonad m where
    touch :: FilePath -> m ()
    mkdir :: FilePath -> m ()
    rm :: FilePath -> m ()
    write :: FilePath -> String -> m ()

-- Define the RealFileSystem monad
newtype RealFileSystem a = RealFileSystem { runRealFileSystem :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- Implement the FileSystemMonad type class for RealFileSystem
instance FileSystemMonad RealFileSystem where
    touch path = RealFileSystem $ void $ runCommandLocally ("touch " ++ path)
    mkdir path = RealFileSystem $ void $ runCommandLocally ("mkdir " ++ path)
    rm path = RealFileSystem $ void $ runCommandLocally ("rm " ++ path)
    write path str = RealFileSystem $ void $ runCommandLocally ("echo " ++ str ++ " >> " ++ path)
