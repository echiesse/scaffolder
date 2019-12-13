module Utils where

import System.IO

withTextFile :: FilePath -> (String -> IO r) -> IO r
withTextFile fileName handler = withFile fileName ReadMode handleContents
    where
        handleContents handle = hGetContents handle >>= handler
