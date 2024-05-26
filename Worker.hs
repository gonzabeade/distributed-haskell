import System.IO (hFlush, stdout, withFile, IOMode(..), hPutStrLn)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectory, listDirectory, removeFile, removeDirectoryRecursive)
import Control.Monad (forM_)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, fromMaybe)
import System.Environment (lookupEnv)


-- Create a simple algebraic type to wrap around commands runnable in the worker 
-- Note: this is different than Log.Command because this allows read-only commands 
data WorkerUICommand
    = Ls FilePath
    | Write String FilePath
    | Touch FilePath
    | Mkdir FilePath
    | Tree FilePath
    | Rm FilePath
    | Unknown
    deriving Show

-- Simple aux function to fetch node name from env. variable
getEnv :: String -> IO String
getEnv id = fromMaybe "Unknown" <$> lookupEnv id

nodeName :: IO String
nodeName = getEnv "NODE_NAME"

-- Main function
-- Node setup and, when ready, begins the shellLoop 
main :: IO ()
main = do
    name <- nodeName
    putStrLn $ "You are running a Haskell Distributed Node!"
    putStrLn $ "Node Name: " ++ name
    rootDir <- getRootDir  -- Get the root directory
    shellLoop rootDir

-- Function to get the root directory - file system namespace 
getRootDir :: IO FilePath
getRootDir = return "root-worker"  

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
        ("ls" : filePath : _)          -> Ls filePath
        ("write" : str : filePath : _) -> Write str filePath
        ("touch" : filePath : _)       -> Touch filePath
        ("mkdir" : filePath : _)       -> Mkdir filePath
        ("tree" : filePath : _)        -> Tree filePath
        ("rm" : filePath : _)          -> Rm filePath
        _                              -> Unknown


executeCommand :: FilePath -> WorkerUICommand -> IO ()

-- Execute command - write operations
executeCommand rootDir (Write str filePath) = do
    let fullPath = rootDir ++ "/" ++ filePath
    withFile fullPath AppendMode (\handle -> hPutStrLn handle str)
    putStrLn $ "Wrote to " ++ fullPath

executeCommand rootDir (Touch filePath) = do
    let fullPath = rootDir ++ "/" ++ filePath
    writeFile fullPath ""
    putStrLn $ "Created file " ++ fullPath

executeCommand rootDir (Mkdir filePath) = do
    let fullPath = rootDir ++ "/" ++ filePath
    createDirectory fullPath
    putStrLn $ "Created directory " ++ fullPath

executeCommand rootDir (Rm filePath) = do
    let fullPath = rootDir ++ "/" ++ filePath
    isFile <- doesFileExist fullPath
    isDir <- doesDirectoryExist fullPath
    if isFile
        then do
            removeFile fullPath
            putStrLn $ "Removed file " ++ fullPath
        else if isDir
            then do
                removeDirectoryRecursive fullPath
                putStrLn $ "Removed directory " ++ fullPath
            else putStrLn $ "Path " ++ fullPath ++ " does not exist."

-- Execute command - read operations
executeCommand rootDir (Ls filePath) = do
    let fullPath = rootDir ++ "/" ++ filePath
    exists <- doesDirectoryExist fullPath
    if exists
    then do
        contents <- listDirectory fullPath
        forM_ contents putStrLn
    else putStrLn $ "Directory " ++ fullPath ++ " does not exist."

executeCommand rootDir (Tree filePath) = do
    let fullPath = rootDir ++ "/" ++ filePath
    exists <- doesDirectoryExist fullPath
    if exists
    then printTree 0 fullPath
    else putStrLn $ "Directory " ++ fullPath ++ " does not exist."

executeCommand _ Unknown = putStrLn "Unknown command"

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
