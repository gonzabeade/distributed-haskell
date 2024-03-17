{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)

-- Define API endpoints
type API = "hello" :> QueryParam "msg" String :> Get '[PlainText] String
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

-- Function to continuously read and log the value from the MVar
logMVar :: MVar String -> IO ()
logMVar mvar = do
  value <- takeMVar mvar
  putStrLn $ "Logged value: " ++ value
  logMVar mvar  -- Recursively call to continuously read and log

-- Create a servant application
app :: MVar String -> Application
app mvar = serve (Proxy :: Proxy API) (server mvar)

-- Main function to start the server
main :: IO ()
main = do
  mvar <- newEmptyMVar
  _ <- forkIO $ logMVar mvar  -- Start the logging thread
  run 8080 (app mvar)
