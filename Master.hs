{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Network.Wai.Handler.Warp (run)
import System.Directory (doesFileExist)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Servant
import Log
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 as BLU


-- Define API endpoints using a strong type 
type API =
       "commands" :> QueryParam "nodeId" String
                  :> QueryParam "cmd" String
                  :> QueryParam "path" String
                  :> QueryParam "content" String
                  :> Post '[PlainText] String
  :<|> "log" :> Get '[PlainText] String  -- New endpoint definition

-- Define handlers for the API endpoints
server :: Server API
server = commandHandler :<|> logHandler  -- Added logHandler
  where
    commandHandler :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Handler String
    commandHandler mNodeId mCmd mPath mContent =
      case (mNodeId, mCmd, mPath) of
        (Just nodeId, Just cmd, Just path) -> do
          -- Create a new LogEntry
          entry <- liftIO $ createLogEntry nodeId (parseCommand cmd path mContent)
          -- Deserialize the log from log.json
          maybeLog <- liftIO $ readLogFromFile "log.json"
          case maybeLog of
            Just log -> do
              -- Append the new LogEntry to the existing Log
              let newLog = appendToLog entry log
              -- Serialize the updated Log back to log.json
              liftIO $ writeLogToFile "log.json" newLog
              return $ "Command executed and logged: " ++ show entry
            Nothing -> return "Failed to read log file"
        _ -> return "Missing parameters"

    logHandler :: Handler String
    logHandler = do
        logContents <- liftIO $ BL.readFile "log.json"
        let jsonContents = BLU.toString logContents 
        return jsonContents

    maybeContent :: Maybe String -> String
    maybeContent Nothing  = ""
    maybeContent (Just c) = ", Content: " ++ c

    parseCommand :: String -> String -> Maybe String -> Command
    parseCommand "touch" path _      = Touch path
    parseCommand "mkdir" path _      = Mkdir path
    parseCommand "rm" path _         = Rm path
    parseCommand "write" path mCont  = Write (maybe "" id mCont) path
    parseCommand _ _ _               = error "Invalid command"

    logEntry :: LogEntry -> Handler ()
    logEntry entry = do
      -- Implement logic to append the log entry to the log
      -- For now, let's just print the entry
      liftIO $ putStrLn $ "Logged entry: " ++ show entry

    readLogFromFile :: FilePath -> IO (Maybe Log)
    readLogFromFile path = deserializeLog <$> readFile path

    writeLogToFile :: FilePath -> Log -> IO ()
    writeLogToFile path log = writeFile path (serializeLog log)

-- Define the API type and server
api :: Proxy API
api = Proxy

main :: IO ()
main = do
  -- Check if the log file exists
  logFileExists <- doesFileExist "log.json"
  
  -- If the log file doesn't exist, create an empty log and serialize it
  unless logFileExists $ do
    let emptyLog = End
    writeLogToFile "log.json" emptyLog
  
  -- Start the server
  run 8080 $ serve api server
