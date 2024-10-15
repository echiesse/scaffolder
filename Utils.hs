{-# LANGUAGE FlexibleInstances #-}
module Utils where

import System.IO
import System.Directory
import qualified Data.Text as T
import System.Exit


-- -----------------------------------------------------------------------------
withTextFile :: FilePath -> (String -> IO r) -> IO r
withTextFile fileName handler = withFile fileName ReadMode handleContents
    where
        handleContents handle = hGetContents handle >>= handler

maybeEnsureDir :: Maybe FilePath -> IO ()
maybeEnsureDir dir = case dir of
    Nothing -> return ()
    (Just path) -> createDirectoryIfMissing True path


-- -----------------------------------------------------------------------------
class Show a => PrintList a where
    printList :: [a] -> IO()
    printList = foldr ((>>) . print) (return ())

instance PrintList String where
    printList = foldr ((>>) . putStrLn) (return ())

instance PrintList Int where


-- -----------------------------------------------------------------------------
strip = T.unpack . T.strip . T.pack


-- -----------------------------------------------------------------------------
appExit :: Int -> String -> IO b
appExit code message = do
    putStrLn message
    if code == 0
        then exitSuccess
        else exitWith (ExitFailure code)
