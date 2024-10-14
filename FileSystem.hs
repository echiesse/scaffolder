module FileSystem where

import System.Directory

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path action = do
    cwd <- getCurrentDirectory
    setCurrentDirectory path
    res <- action
    setCurrentDirectory cwd
    return res


isEmptyDir :: FilePath -> IO Bool
isEmptyDir path = do
    fmap null (listDirectory path)
