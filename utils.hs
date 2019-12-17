module Utils where

import System.IO
import System.Directory

withTextFile :: FilePath -> (String -> IO r) -> IO r
withTextFile fileName handler = withFile fileName ReadMode handleContents
    where
        handleContents handle = hGetContents handle >>= handler

ensureDir :: Maybe FilePath -> IO ()
ensureDir dir = case dir of
    Nothing -> return ()
    (Just path) -> createDirectoryIfMissing True path
