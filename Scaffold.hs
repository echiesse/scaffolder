module Scaffold where

import qualified Control.Monad
import Data.Maybe (fromMaybe)
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import FileSystem
import Parser
import ProgramInfo
import ScaffoldBuilder
import ScaffoldTree


noAction = return ()

makeIndent level = take (level * length indentation) $ cycle indentation

touch fileName = withFile fileName WriteMode (\handle -> return ())

mkdir = createDirectory

pprint :: ScaffoldTree -> IO ()
pprint doc = traverseTree doc 0 pprintBuilder

pprintBuilder :: ScaffoldBuilder (IO ())
pprintBuilder = ScaffoldBuilder {
    buildFile = \file level -> putStrLn $ makeIndent level ++ "- " ++ sfFileName file,
    buildDir  = \(SfDir name subitems) level -> do
        putStrLn $ makeIndent level ++ "+ " ++ name
        case subitems of
            [] -> noAction
            (item:items) -> traverseTree subitems (level + 1) pprintBuilder
}


fsBuilder :: ScaffoldBuilder (IO ())
fsBuilder = ScaffoldBuilder {
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
        cdBaseDir = do
            let targetPath = fromMaybe "." baseDir
            exitIfNotEmptyDir targetPath
            setCurrentDirectory targetPath


exitIfNotEmptyDir path = do
    isEmpty <- isEmptyDir path
    Control.Monad.unless isEmpty $ die "Directory must be empty"


ensureScaffolderDir :: IO FilePath
ensureScaffolderDir = do
    scaffolderDirPath <- getAppDataPath
    createDirectoryIfMissing False scaffolderDirPath

    templatesPath <- getTemplatesDirPath
    createDirectoryIfMissing False templatesPath

    return scaffolderDirPath
