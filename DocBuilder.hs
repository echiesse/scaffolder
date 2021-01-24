module DocBuilder where

import ScaffoldTree

type ItemBuilder a = SfItem -> Int -> a
data DocBuilder a = DocBuilder {
        buildFile :: ItemBuilder a,
        buildDir :: ItemBuilder a
    }

traverseAST :: ScaffoldTree -> Int -> DocBuilder (IO ()) -> IO ()
traverseAST ast level docBuilder = mapM_ (stepAST level docBuilder) ast


stepAST :: Int -> DocBuilder (IO ()) -> SfItem -> IO ()
stepAST level builder item = do
    case item of
        (SfFile name) -> (buildFile builder) item level
        (SfDir name subitems) -> (buildDir builder) item level
