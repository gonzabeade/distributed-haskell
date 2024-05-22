module Log2Monad (
    applyLogs
) where

import FileSystemMonad
import Log

-- Aux function that maps a LogEntry to a FileSystemMonad action
applyLogEntry :: FileSystemMonad m => LogEntry -> m () -> m ()
applyLogEntry (LogEntry _ _ (Touch path)) = \m -> touch path >> m
applyLogEntry (LogEntry _ _ (Mkdir path)) = \m -> mkdir path >> m
applyLogEntry (LogEntry _ _ (Rm path)) = \m -> rm path >> m
applyLogEntry (LogEntry _ _ (Write path str)) = \m -> write path str >> m

-- Define a function that translates a Log structure to its monadic interpretation
-- Notice the use of the right-folding function for the Log structure
applyLogs :: FileSystemMonad m => Log -> m ()
applyLogs log = foldrLog applyLogEntry (return ()) log