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



-- Define API endpoints
type API = 
           "hello" :> QueryParam "msg" String :> Get '[PlainText] String
      :<|> "bye"   :> QueryParam "msg" String :> Get '[PlainText] String

-- Define server handlers
server :: MVar String -> Server API
server mvar = helloHandler :<|> byeHandler
  where
    helloHandler :: Maybe String -> Handler String
    helloHandler (Just msg) = do
      liftIO $ putMVar mvar ("Hello, " ++ msg ++ "!")
      return $ "Hello, " ++ msg ++ "!"

    byeHandler :: Maybe String -> Handler String
    byeHandler (Just msg) = do
      liftIO $ putMVar mvar ("Bye, " ++ msg ++ "!")
      return $ "Bye, " ++ msg ++ "!"

-- Create a servant application
app :: MVar String -> Application
app mvar = serve (Proxy :: Proxy API) (server mvar)


-- Function to continuously read and log the value from the MVar
-- It blocks whenever the MVar is empty, there is nothing to do 
-- This function is to be run in a thread and orchestrates Raft messages 
-- for this node
runRaftMainLoop :: MVar String -> IO ()
runRaftMainLoop mvar = do
  value <- takeMVar mvar
  putStrLn $ "Logged value: " ++ value
  runRaftMainLoop mvar  -- Recursively call to continuously read and log




-- When the Raft node starts or restores from crash, 
-- it starts here 
main :: IO ()
main = do
  maybeConfig <- loadRaftConfigFromDisk "node_config.json"

  case maybeConfig of
    Just config -> writeRaftConfigToDisk config "node_config_2.json"
    Nothing -> putStrLn "Failed to load config"




  -- -- Launch Raft
  -- mvar <- newEmptyMVar
  -- _ <- forkIO $ runRaftMainLoop mvar  -- Start the logging thread
  -- run 8080 (app mvar)
