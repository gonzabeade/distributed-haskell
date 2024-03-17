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

import RaftUtils


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
runRaftMainLoop :: MVar Dog -> IO ()
runRaftMainLoop mvar = do
  value <- takeMVar mvar
  putStrLn $ "Logged value: " ++ show value
  runRaftMainLoop mvar  -- Recursively call to continuously read and log




-- When the Raft node starts or restores from crash, 
-- it starts here 
main :: IO ()
main = do

  -- Initialisation - Recover config from disk or default on first startup 
  maybeConfig <- loadRaftConfigFromDisk "node_config.json"

  case maybeConfig of
    Just config -> do
      modifiedConfig <- setUncommittedLog config (Log 4 "Hello" (Log 7 "Bye" EmptyLog))
      modifiedConfig <- setCurrentTerm config 56
      modifiedConfig <- setCommitLength config 67
      modifiedConfig <- setVotedFor config (Just $ Node "192.123.67.87")


      print modifiedConfig


  -- Launch Raft
  -- mvar <- newEmptyMVar
  -- _ <- forkIO $ runRaftMainLoop mvar  -- Start the logging thread
  -- run 8080 (app mvar)
