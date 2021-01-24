module Scaffold where

-- import Data.List
-- import Data.String.Utils
import System.IO
import System.Directory

import DocBuilder
import SfAST
import Parser


noAction = return ()

makeIndent level = take (level * length indentation) $ cycle indentation

touch fileName = withFile fileName WriteMode (\handle -> return ())

mkdir = createDirectory

pprint :: SfAST -> IO ()
pprint doc = traverseAST doc 0 pprintBuilder

pprintBuilder :: DocBuilder (IO ())
pprintBuilder = DocBuilder{
    buildFile = \file level -> putStrLn $ makeIndent level ++ "- " ++ sfdocFileName file,
    buildDir  = \(SfDir name subitems) level -> do
        putStrLn $ makeIndent level ++ "+ " ++ name
        case subitems of
            [] -> noAction
            (item:items) -> traverseAST subitems (level + 1) pprintBuilder
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


scaffoldTree :: SfAST -> IO()
scaffoldTree doc = traverseAST doc 0 fsBuilder


scaffold :: Maybe FilePath -> String -> IO ()
scaffold baseDir input = cdBaseDir >> (scaffoldTree . parseDoc) input
    where
        cdBaseDir = case baseDir of
            Nothing -> return ()
            (Just path) -> setCurrentDirectory path
