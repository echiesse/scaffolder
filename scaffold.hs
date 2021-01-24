module Scaffold where

-- import Data.List
-- import Data.String.Utils
import System.IO
import System.Directory
import System.FilePath

import qualified Config
import DocBuilder
import ScaffoldTree
import Parser


noAction = return ()

makeIndent level = take (level * length indentation) $ cycle indentation

touch fileName = withFile fileName WriteMode (\handle -> return ())

mkdir = createDirectory

pprint :: ScaffoldTree -> IO ()
pprint doc = traverseTree doc 0 pprintBuilder

pprintBuilder :: DocBuilder (IO ())
pprintBuilder = DocBuilder {
    buildFile = \file level -> putStrLn $ makeIndent level ++ "- " ++ sfFileName file,
    buildDir  = \(SfDir name subitems) level -> do
        putStrLn $ makeIndent level ++ "+ " ++ name
        case subitems of
            [] -> noAction
            (item:items) -> traverseTree subitems (level + 1) pprintBuilder
}


fsBuilder :: DocBuilder (IO ())
fsBuilder = DocBuilder {
    buildFile = \(SfFile name) _ -> touch name,
    buildDir = \(SfDir name subitems) _ -> do
        mkdir name
        case subitems of
            [] -> noAction
            _ -> do
                setCurrentDirectory name
                scaffoldTree subitems
                setCurrentDirectory ".."
}


scaffoldTree :: ScaffoldTree -> IO ()
scaffoldTree doc = traverseTree doc 0 fsBuilder


scaffold :: Maybe FilePath -> String -> IO ()
scaffold baseDir input = cdBaseDir >> (scaffoldTree . parseDoc) input
    where
        cdBaseDir = case baseDir of
            Nothing -> return ()
            (Just path) -> setCurrentDirectory path


ensureScaffolderDir :: IO FilePath
ensureScaffolderDir = do
    scaffolderDirPath <- getAppUserDataDirectory Config.dataDirName
    createDirectoryIfMissing False scaffolderDirPath
    createDirectoryIfMissing False $ joinPath [scaffolderDirPath, Config.templatesSubdir]
    return scaffolderDirPath
