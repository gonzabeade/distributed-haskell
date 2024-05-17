{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (doesFileExist) 
import Data.List.Split (splitOn)
import System.Process
import System.IO (hGetContents)


import RaftUtils


runCommandLocally :: String -> IO String
runCommandLocally cmd = do
    (_, Just hout, _, _) <- createProcess (shell cmd) { std_out = CreatePipe }
    output <- hGetContents hout
    return output


-- Define the RaftMessage possible types 
data RaftMessage = 
    VoteRequest Node TermNumber Int TermNumber | 
    VoteResponse Node TermNumber Bool |
    BroadcastRequest String | 
    LogRequest Node TermNumber Int TermNumber Int | 
    LogResponse Node TermNumber 


data Dog = Chipi String | Maggie String deriving Show


-- Define API endpoints
type API = 
           "hello" :> QueryParam "msg" String :> Get '[PlainText] String
      :<|> "bye"   :> QueryParam "msg" String :> Get '[PlainText] String

-- Define server handlers
server :: MVar Dog -> Server API
server mvar = helloHandler :<|> byeHandler
  where

    helloHandler :: Maybe String -> Handler String
    helloHandler (Just msg) = do
      liftIO $ putMVar mvar (Chipi msg)
      return $ "Hello, " ++ msg ++ "!"

    byeHandler :: Maybe String -> Handler String
    byeHandler (Just msg) = do
      liftIO $ putMVar mvar (Maggie msg)
      return $ "Bye, " ++ msg ++ "!"

-- Create a servant application
app :: MVar Dog -> Application
app mvar = serve (Proxy :: Proxy API) (server mvar)


-- Function to continuously read and log the value from the MVar
-- It blocks whenever the MVar is empty, there is nothing to do 
-- This function is to be run in a thread and orchestrates Raft messages 
-- for this node
runRaftMainLoop :: MVar Dog -> RaftConfig -> IO ()
runRaftMainLoop mvar config = do
  print config
  value <- takeMVar mvar
  putStrLn $ "Logged value: " ++ show value
  runCommandLocally $ "touch " ++ show value 
  runRaftMainLoop mvar config -- Recursively call to continuously read and log



-- When the Raft node starts or restores from crash, 
-- it starts here 
main :: IO ()
main = do

  -- Initialisation - Recover config from disk or default on first startup 
  maybeConfig <- loadRaftConfigFromDisk "node_config.json"
  
  do
    config <- case maybeConfig of
        Just c -> do
            modified1 <- setUncommittedLog c (Log 4 "Hello" (Log 7 "Bye" EmptyLog))
            modified2 <- setCurrentTerm modified1 56
            modified3 <- setCommitLength modified2 67
            modified4 <- setVotedFor modified3 (Just $ Node "192.123.67.87")
            return modified4
        Nothing -> error "Configuration is missing"

    -- Launch Raft
    mvar <- newEmptyMVar
    _ <- forkIO $ runRaftMainLoop mvar config  -- Start the logging thread
    run 8080 (app mvar)
