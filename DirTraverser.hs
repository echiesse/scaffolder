module DirTraverser where

import ScaffoldTree
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath


traverseDir :: FilePath -> IO ScaffoldTree
traverseDir path = fmap reverse $ listDirectory path >>= traverseDirContents path

traverseDirContents :: FilePath -> [FilePath] -> IO ScaffoldTree
traverseDirContents _ [] = return []
traverseDirContents basePath (item: items) = do
    traversed <- traverseDirContents basePath items
    isDirectory <- doesDirectoryExist $ joinPath [basePath, item]
    if isDirectory
        then do
            dirContents <- traverseDir $ joinPath [basePath, item]
            return $ SfDir {sfDirName = item, sfDirContents = dirContents} : traversed
        else do
            return $ SfFile item: traversed
