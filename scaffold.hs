module Scaffold where

-- import Data.List
-- import Data.String.Utils
import System.IO
import System.Directory

import SfAST
import Parser

type ParserAction = SfItem -> Int -> IO ()

noAction = return ()

type ItemBuilder a = SfItem -> Int -> a
data DocBuilder a = DocBuilder {
        buildFile :: ItemBuilder a,
        buildDir :: ItemBuilder a
    }

pprintBuilder :: DocBuilder (IO ())
pprintBuilder = DocBuilder{
    buildFile = \file level -> putStrLn $ makeIndent level ++ "- " ++ sfdocFileName file,
    buildDir  = \(SfDir name subitems) level -> do
        putStrLn $ makeIndent level ++ "+ " ++ name
        case subitems of
            [] -> noAction
            (item:items) -> traverseAST subitems (level + 1) pprintBuilder
}

traverseAST :: SfAST -> Int -> DocBuilder (IO ()) -> IO ()
traverseAST [] level _ = noAction
traverseAST (item: items) level docBuilder = do
    case item of
        (SfFile name) -> (buildFile docBuilder) item level
        (SfDir name subitems) -> (buildDir docBuilder) item level
    traverseAST items level docBuilder

pprint :: SfAST -> IO ()
pprint doc = traverseAST doc 0 pprintBuilder

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

touch fileName = withFile fileName WriteMode (\handle -> return ())

mkdir = createDirectory

scaffold = scaffoldTree . parseDoc
