{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.IO (hFlush, stdout, withFile, IOMode(..), hPutStrLn)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectory, listDirectory, removeFile, removeDirectoryRecursive, setCurrentDirectory)
import Control.Monad (forM_)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, fromMaybe)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.Process


-- Internal imports 
import Log
import Log2Monad
import FileSystemMonad

-- Webhook imports
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Servant
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Data.Word (Word8)
import Data.Char (ord)

import qualified Data.Text.IO as TIO
import Data.Text (Text)
import System.IO (IOMode(..), withFile)

-- Webhook functions
type LogAPI = "log" :> ReqBody '[PlainText] Text :> Post '[PlainText] NoContent

logHandler :: FilePath -> FilePath -> Text -> Handler NoContent
logHandler logPath pwd payload = do
    liftIO $ do
        putStrLn "<New Log from master received>"
        withFile logPath WriteMode (\h -> TIO.hPutStrLn h payload)
        maybeLog <- readLogFromFile logPath
        let log = fromMaybe End maybeLog
        putStrLn $ "Log: " ++ show maybeLog
        removeDirectoryRecursive pwd
        createDirectory pwd
        setCurrentDirectory pwd
        runRealFileSystem (applyLogs log)
        setCurrentDirectory ".."    
    return NoContent

logAPI :: Proxy LogAPI
logAPI = Proxy

server :: FilePath -> FilePath -> Server LogAPI
server logJson rootDir = logHandler logJson rootDir

app :: FilePath -> FilePath -> Application
app logJson rootDir = serve logAPI (server logJson rootDir)


-- Create a simple algebraic type to wrap around commands runnable in the worker 
-- Note: this is different than Log.Command because this allows read-only commands 
data WorkerUICommand
    = UILs FilePath
    | UIWrite String FilePath
    | UITouch FilePath
    | UIMkdir FilePath
    | UITree FilePath
    | UIRm FilePath
    | UIUnknown
    deriving Show

-- Simple aux function to fetch node name from env. variable
getEnv :: String -> IO String
getEnv id = fromMaybe "Unknown" <$> lookupEnv id

nodeName :: IO String
nodeName = getEnv "NODE_NAME"

-- Function to get the root directory - file system namespace 
getRootDir :: IO FilePath
getRootDir = return "root-worker"  

-- Function to get the log json 
getLogJson :: IO FilePath
getLogJson = return "log-node.json"  

-- Main function
-- Initialize webhook thread to listen to changes from Master 
-- Node setup and, when ready, begins the shellLoop 
-- Main function to run the server
main :: IO ()
main = do
    name <- nodeName
    putStrLn $ "You are running a Haskell Distributed Node!"
    putStrLn $ "Node Name: " ++ name
    rootDir <- getRootDir  -- Get the root directory
    logJson <- getLogJson  -- Get the root directory

    -- Spawn a new thread for the servant server
    _ <- forkIO $ run 8080 (app logJson rootDir)  -- Change the port number as needed

    shellLoop rootDir

-- Function to make an API call POST 
makeAPIpost :: String -> Int -> String -> String -> String -> String -> Int -> IO String
makeAPIpost host port cmd path content nodeId lastLogId = do
    let url = "http://" ++ host ++ ":" ++ show port ++ "/commands?cmd=" ++ cmd ++ "&path=" ++ path ++ "&content=" ++ content ++ "&nodeId=" ++ nodeId ++ "&lastLogId=" ++ show lastLogId
    let args = ["-X", "POST", url, "-v"] 
    readProcess "curl" args ""

-- Function to make an API call
makeAPIget :: String -> Int -> IO String
makeAPIget host port = do
    let url = "http://" ++ host ++ ":" ++ show port ++ "/log"
    let args = ["-X", "GET", url, "-v", "--output", "log-request.json"]
    readProcess "curl" args ""

-- Run the shellLoop
shellLoop :: FilePath -> IO ()
shellLoop rootDir = do
    putStr "shell> "
    hFlush stdout
    input <- getLine
    let command = parseCommand input
    if input == "exit" || input == "quit"
    then putStrLn "Goodbye!"
    else do
        executeCommand rootDir command
        shellLoop rootDir

parseCommand :: String -> WorkerUICommand
parseCommand input =
    case words input of
        ("ls" : filePath : _)          -> UILs filePath
        ("write" : str : filePath : _) -> UIWrite str filePath
        ("touch" : filePath : _)       -> UITouch filePath
        ("mkdir" : filePath : _)       -> UIMkdir filePath
        ("tree" : filePath : _)        -> UITree filePath
        ("rm" : filePath : _)          -> UIRm filePath
        _                              -> UIUnknown


executeCommand :: FilePath -> WorkerUICommand -> IO ()

-- Execute command - write operations
executeCommand rootDir (UIWrite str filePath) = do
    _ <- makeAPIpost "haskell-master" 8080 "write" filePath str "nodeIdHolaHola" 56
    return ()

executeCommand rootDir (UITouch filePath) = do
    _ <- makeAPIpost "haskell-master" 8080 "touch" filePath "" "nodeIdHolaHola" 56
    return ()

executeCommand rootDir (UIMkdir filePath) = do
    _ <- makeAPIpost "haskell-master" 8080 "mkdir" filePath "" "nodeIdHolaHola" 56
    return ()

executeCommand rootDir (UIRm filePath) = do
    _ <- makeAPIpost "haskell-master" 8080 "rm" filePath "" "nodeIdHolaHola" 56
    return ()

-- Execute command - read operations
executeCommand rootDir (UILs filePath) = do
    let fullPath = rootDir ++ "/" ++ filePath
    exists <- doesDirectoryExist fullPath
    if exists
    then do
        contents <- listDirectory fullPath
        forM_ contents putStrLn
    else putStrLn $ "Directory " ++ fullPath ++ " does not exist."

executeCommand rootDir (UITree filePath) = do
    let fullPath = rootDir ++ "/" ++ filePath
    exists <- doesDirectoryExist fullPath
    if exists
    then printTree 0 fullPath
    else putStrLn $ "Directory " ++ fullPath ++ " does not exist."

executeCommand _ UIUnknown = putStrLn "Unknown command"

-- Aux function to print directory in tree format
printTree :: Int -> FilePath -> IO ()
printTree indent path = do
    contents <- listDirectory path
    forM_ contents $ \name -> do
        let fullPath = path ++ "/" ++ name
        putStrLn $ replicate (indent * 2) ' ' ++ name
        isDir <- doesDirectoryExist fullPath
        if isDir
        then printTree (indent + 1) fullPath
        else return ()
