{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- External imports 
import Network.Wai.Handler.Warp (run)
import System.Directory (doesFileExist)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Servant
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)

-- Internal imports 
import Log

-- Define API endpoints using a strong type 
type API =
       "commands" :> QueryParam "nodeId" String
                  :> QueryParam "cmd" String
                  :> QueryParam "path" String
                  :> QueryParam "content" String
                  :> QueryParam "lastLogId" String
                  :> Post '[PlainText] String
  :<|> "log" :> Get '[PlainText] String

-- Simple function to parse a string argument into a Command 
parseCommand :: String -> String -> Maybe String -> Command
parseCommand "touch" path _      = Touch path
parseCommand "mkdir" path _      = Mkdir path
parseCommand "rm" path _         = Rm path
parseCommand "write" path mCont  = Write (maybe "" id mCont) path
parseCommand _ _ _               = error "Invalid command"

-- Handler - POST /commands
commandHandler :: MVar Command -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Handler String
commandHandler mvar (Just nodeId) (Just cmd) (Just path) content _ = do
    let parsedCommand = parseCommand cmd path content
    liftIO $ putMVar mvar parsedCommand
    return $ "Command received and processed: " ++ show parsedCommand
commandHandler _ _ _ _ _ _ = return "Missing required parameters"

-- Handler - GET /log
logHandler :: Handler String
logHandler = liftIO $ do
    logContents <- BL.readFile "log.json"
    return $ BLU.toString logContents

-- Run main loop 
-- Function to continuously read and log the value from the MVar
-- It blocks whenever the MVar is empty, there is nothing to do 
-- This function is to be run in a thread and orchestrates messages 
runMainLoop :: FilePath -> MVar Command -> IO ()
runMainLoop logFilePath mvar = do
  -- Deserialize the log from file
  maybeLog <- readLogFromFile logFilePath
  let log = fromMaybe End maybeLog
  
  -- Take a command from the MVar
  command <- takeMVar mvar

  -- Create a log entry with a random UUID
  logEntry <- createLogEntry "node" command

  -- Append the new log entry to the log
  let newLog = appendToLog logEntry log
  
  -- Log the value
  putStrLn $ "Logged value: " ++ show newLog

  -- Serialize new log
  writeLogToFile logFilePath newLog

  -- Recur with the updated log
  runMainLoop logFilePath mvar

-- Start the server
api :: Proxy API
api = Proxy

server :: MVar Command -> Server API
server mvar = commandHandler mvar :<|> logHandler

-- Main 
main :: IO ()
main = do

  -- Check if the log file exists
  logFileExists <- doesFileExist "log.json"
  
  -- If the log file doesn't exist, create an empty log and serialize it
  unless logFileExists $ do
    let emptyLog = End
    writeLogToFile "log.json" emptyLog

  -- Initialize mvar 
  mvar <- newEmptyMVar
  _ <- forkIO $ runMainLoop "log.json" mvar

  -- Initialize server
  run 8080 $ serve api (server mvar)
