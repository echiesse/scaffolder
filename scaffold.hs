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
traverseAST ast level docBuilder = mapM_ (stepAST level docBuilder) ast

stepAST :: Int -> DocBuilder (IO ()) -> SfItem -> IO ()
stepAST level builder item = do
    case item of
        (SfFile name) -> (buildFile builder) item level
        (SfDir name subitems) -> (buildDir builder) item level

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

scaffold :: Maybe FilePath -> String -> IO ()
scaffold baseDir input = cdBaseDir >> (scaffoldTree . parseDoc) input
    where
        cdBaseDir = case baseDir of
            Nothing -> return ()
            (Just path) -> setCurrentDirectory path
