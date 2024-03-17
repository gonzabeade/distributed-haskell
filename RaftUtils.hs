{-# LANGUAGE DeriveGeneric #-}

module RaftUtils (
    RaftConfig(..),
    loadRaftConfigFromDisk,
    writeRaftConfigToDisk
) where

-- Imports 
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import Data.Set (Set)
import qualified Data.Set as Set


-- Definition of data types 

data Log = Log Int String Log | EmptyLog deriving (Show, Generic)
instance ToJSON Log
instance FromJSON Log

data Role = Follower | Leader | Candidate deriving (Show, Generic)
instance ToJSON Role
instance FromJSON Role

data Node = Node String deriving (Show, Generic)
instance ToJSON Node
instance FromJSON Node

type TermNumber = Int 
type CommitLength = Int 
type SentLogLength = [(Node, Int)]
type AckedLogLength = [(Node, Int)]

-- Stable storage for the Raft algorithm 
-- Some values need to be persisted to disk at all times 
-- We implement simple methods that allow us to retrieve and 
-- persist these stable configs as files. 
data RaftStableConfig = RaftStableConfig
    { 
      sCurrentTerm      :: TermNumber
    , sVotedFor         :: Maybe Node
    , sUncommitedLog    :: Log
    , sCommitLength     :: CommitLength
    } deriving (Show, Generic)

instance ToJSON RaftStableConfig
instance FromJSON RaftStableConfig

serializeStableConfig :: RaftStableConfig -> BL.ByteString
serializeStableConfig = encode

deserializeStableConfig :: BL.ByteString -> Maybe RaftStableConfig
deserializeStableConfig = decode


-- The whole set of variables needs to be accesible during execution 
-- Some of these are stable, some others are volatile 
-- We define a homogeneous type that combines both type of variables 
data RaftConfig = RaftConfig
    { 
      currentTerm       :: TermNumber
    , votedFor          :: Maybe Node
    , uncommitedLog     :: Log
    , commitLength      :: CommitLength
    , currentRole       :: Role 
    , currentLeader     :: Maybe Node 
    , votesReceived     :: Set Node
    , sentLength        :: SentLogLength
    , ackedLength       :: AckedLogLength
    } deriving (Show, Generic)

-- When a node is created, it loads with this default config 
defaultRaftConfig :: RaftConfig
defaultRaftConfig = RaftConfig
    { currentTerm = 0
    , votedFor = Nothing
    , uncommitedLog = EmptyLog
    , commitLength = 0
    , currentRole = Follower
    , currentLeader = Nothing
    , votesReceived = Set.empty
    , sentLength = []
    , ackedLength = []
    }

-- Given a config path, it deserializes the file into a RaftConfig
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

-- Serializes a RaftConfig into a file
writeRaftConfigToDisk :: RaftConfig -> FilePath -> IO ()
writeRaftConfigToDisk stableConfig path = do
    let serializedConfig = serializeStableConfig (convertToRaftStableConfig stableConfig)
    BL.writeFile path serializedConfig

-- Helper function that converts a RaftStableConfig into a RaftConfig by adding default values 
convertToRaftConfig :: RaftStableConfig -> RaftConfig
convertToRaftConfig stableConfig = RaftConfig
    { currentTerm = sCurrentTerm stableConfig
    , votedFor = sVotedFor stableConfig
    , uncommitedLog = sUncommitedLog stableConfig
    , commitLength = sCommitLength stableConfig
    , currentRole = Follower    
    , currentLeader = Nothing   
    , votesReceived = Set.empty 
    , sentLength = []
    , ackedLength = []
    }

-- Helper function that converts a RaftConfig into a RaftStableConfig by erasing volatile values 
convertToRaftStableConfig :: RaftConfig -> RaftStableConfig
convertToRaftStableConfig raftConfig = RaftStableConfig
    { sCurrentTerm = currentTerm raftConfig
    , sVotedFor = votedFor raftConfig
    , sUncommitedLog = uncommitedLog raftConfig
    , sCommitLength = commitLength raftConfig
    }