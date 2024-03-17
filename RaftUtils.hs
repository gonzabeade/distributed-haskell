{-# LANGUAGE DeriveGeneric #-}

module RaftUtils (
    RaftConfig(..),
    loadRaftConfigFromDisk,
    writeRaftConfigToDisk
) where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)


data Log = Log Int String Log | EmptyLog deriving (Show, Generic)
instance ToJSON Log
instance FromJSON Log

data Role = Follower | Leader | Candidate deriving (Show, Generic)
instance ToJSON Role
instance FromJSON Role

data Node = Node String deriving (Show, Generic)
instance ToJSON Node
instance FromJSON Node

data RaftStableConfig = RaftStableConfig
    { 
      sCurrentTerm :: Int
    , sVotedFor :: Maybe Node
    , sUncommitedLog :: Log
    , sCommitLength :: Int
    } deriving (Show, Generic)

instance ToJSON RaftStableConfig
instance FromJSON RaftStableConfig

serializeStableConfig :: RaftStableConfig -> BL.ByteString
serializeStableConfig = encode

deserializeStableConfig :: BL.ByteString -> Maybe RaftStableConfig
deserializeStableConfig = decode

data RaftConfig = RaftConfig
    { 
      currentTerm :: Int
    , votedFor :: Maybe Node
    , uncommitedLog :: Log
    , commitLength :: Int
    , currentRole :: Role 
    , currentLeader :: Maybe Node 
    , votesReceived :: [Node] -- Check 
    , sentLength :: [String] -- Check 
    , ackedLength :: [String] -- Check 
    } deriving (Show, Generic)

-- Define a default RaftConfig instance
defaultRaftConfig :: RaftConfig
defaultRaftConfig = RaftConfig
    { currentTerm = 0
    , votedFor = Nothing
    , uncommitedLog = EmptyLog
    , commitLength = 0
    , currentRole = Follower
    , currentLeader = Nothing
    , votesReceived = []
    , sentLength = []
    , ackedLength = []
    }

loadRaftConfigFromDisk :: FilePath -> IO (Maybe RaftConfig)
loadRaftConfigFromDisk path = do
    fileExists <- doesFileExist path
    if fileExists
        then do
            contents <- BL.readFile path
            case deserializeStableConfig contents of
                Just stableConfig -> 
                    return $ Just $ convertToRaftConfig stableConfig
                Nothing -> return Nothing
        else return $ Just defaultRaftConfig


writeRaftConfigToDisk :: RaftConfig -> FilePath -> IO ()
writeRaftConfigToDisk stableConfig path = do
    let serializedConfig = serializeStableConfig (convertToRaftStableConfig stableConfig)
    BL.writeFile path serializedConfig

-- Convert RaftStableConfig to RaftConfig, filling in default values for missing fields
convertToRaftConfig :: RaftStableConfig -> RaftConfig
convertToRaftConfig stableConfig = RaftConfig
    { currentTerm = sCurrentTerm stableConfig
    , votedFor = sVotedFor stableConfig
    , uncommitedLog = sUncommitedLog stableConfig
    , commitLength = sCommitLength stableConfig
    , currentRole = Follower -- Default value for currentRole
    , currentLeader = Nothing -- Default value for currentLeader
    , votesReceived = [] -- Default value for votesReceived
    , sentLength = [] -- Default value for sentLength
    , ackedLength = [] -- Default value for ackedLength
    }

convertToRaftStableConfig :: RaftConfig -> RaftStableConfig
convertToRaftStableConfig raftConfig = RaftStableConfig
    { sCurrentTerm = currentTerm raftConfig
    , sVotedFor = votedFor raftConfig
    , sUncommitedLog = uncommitedLog raftConfig
    , sCommitLength = commitLength raftConfig
    }