{-# LANGUAGE DeriveGeneric #-}

module Log (
    Command(..),    -- Exporting the Command type and its constructors
    LogEntry(..),   -- Exporting the LogEntry type and its constructors
    Log(..),        -- Exporting the Log type and its constructors
    foldrLog,       -- Exporting the foldrLog function
    serializeLog,   -- Exporting the serializeLog function
    deserializeLog, -- Exporting the deserializeLog function
    createLogEntry  -- Exporting a function to create logs with random UUID
) where

import Data.Aeson
import GHC.Generics (Generic)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.ByteString.Lazy.UTF8 (toString, fromString)


-- Supported commands for this implementation
data Command = 
            Touch FilePath              -- Create a new file at file path 
             | Mkdir FilePath           -- Create a new dir at file path
             | Rm FilePath              -- Remove a file or directory at file path 
             | Write String FilePath    -- Write a string to a file path 
             deriving (Show, Eq, Generic)

-- The minimal unit that can be stored in a Log
data LogEntry = LogEntry {
    entryId :: UUID,                    -- Each entry is uniquely identified by an id 
    node :: String,                     -- Each entry contains the author node of the command
    command :: Command                  -- Each entry contains a command to be run 
} deriving (Show, Eq, Generic)

-- A log is simply a linear collection of LogEntries 
-- It could be modeled with a [LogEntry], but we use an algebraic structure 
-- to try out what was learnt in class
data Log = 
        LogEntryNode LogEntry Log
         | End
         deriving (Show, Eq, Generic)

-- Define the foldrLog function 
-- Performs a right-fold operation over the Log structure
foldrLog :: (LogEntry -> b -> b) -> b -> Log -> b
foldrLog _ z End = z
foldrLog f z (LogEntryNode entry rest) = f entry (foldrLog f z rest)

-- We must allow a Log structure to be serialized and deserialized 
-- We don't care much about the inner structure
-- We delegate this task to the library 
instance ToJSON Command
instance FromJSON Command
instance ToJSON LogEntry
instance FromJSON LogEntry
instance ToJSON Log
instance FromJSON Log

-- Serialize a Log structure
serializeLog :: Log -> String
serializeLog log = toString $ encode log

-- Deserialize a Log structure
deserializeLog :: String -> Maybe Log
deserializeLog str = decode $ fromString str

-- Create a log entry with a random UUID generated at construction time
-- Returns IO because it has a "side effect"
-- Code is neater
-- I could create the UUID before and just use the pure LogEntry constructor 
createLogEntry :: String -> Command -> IO LogEntry
createLogEntry author cmd = do
    uuid <- nextRandom
    return $ LogEntry uuid author cmd