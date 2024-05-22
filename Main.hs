{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}




module Main where


-- Internal imports 
import Log
import FileSystemMonad
import Log2Monad

-- External imports
import Data.UUID.V4 (nextRandom)
import qualified Data.ByteString.Lazy as B

-- Main function for testing
main :: IO ()
main = do

    -- Create log entries with random UUIDs
    entry1 <- createLogEntry "node1" (Touch "./sandbox/newdir/newfile1")
    entry2 <- createLogEntry "node1" (Write "./sandbox/newdir/newfile1" "myfile")
        
    -- Create an initial empty log
    let log = End
    
    -- Append entries to the log
    let log' = LogEntryNode entry1 log
    let log'' = LogEntryNode entry2 log'
    
    -- Serialize the log to JSON
    let serializedLog = serializeLog log''
    
    -- Print the serialized log
    putStrLn "Serialized Log:"
    print serializedLog
    
    -- Deserialize the JSON back to a Log value
    let maybeDeserializedLog = deserializeLog serializedLog
    
    -- Print the deserialized log
    putStrLn "\nDeserialized Log:"
    print (maybeDeserializedLog :: Maybe Log)

    -- Apply logs in the RealFileSystem monad
    putStrLn "\nApplying Logs:"

    -- Apply logs in the RealFileSystem monad
    putStrLn "\nApplying Logs:"
    result <- runRealFileSystem $ applyLogs log''
    print result