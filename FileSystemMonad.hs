{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileSystemMonad (
    runCommandLocally,
    FileSystemMonad(..),
    RealFileSystem(..), 
    OutputFileSystem(..)
) where

import System.IO (hGetContents)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Process
import qualified Data.Map as Map
import System.Exit
import Control.Exception (evaluate)

-- Function that allows strings to be run as commands in the current shell context
runCommandLocally :: String -> IO String
runCommandLocally cmd = do
    (_, Just hout, _, ph) <- createProcess (shell cmd) { std_out = CreatePipe }
    output <- hGetContents hout
    exitCode <- waitForProcess ph
    -- Ensure the process has exited successfully
    case exitCode of
        ExitSuccess   -> return output
        ExitFailure _ -> error "Command failed to execute successfully"

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

-- Define the OutputFileSystem monad
newtype OutputFileSystem a = OutputFileSystem { runOutputFileSystem :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- Implement the FileSystemMonad type class for OutputFileSystem
instance FileSystemMonad OutputFileSystem where
    touch path = OutputFileSystem $ putStrLn ("touch " ++ path)
    mkdir path = OutputFileSystem $ putStrLn ("mkdir " ++ path)
    rm path = OutputFileSystem $ putStrLn ("rm " ++ path)
    write path str = OutputFileSystem $ putStrLn ("echo " ++ str ++ " >> " ++ path)
