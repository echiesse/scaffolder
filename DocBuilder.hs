module DocBuilder where

import ScaffoldTree

type ItemBuilder a = SfItem -> Int -> a
data DocBuilder a = DocBuilder {
        buildFile :: ItemBuilder a,
        buildDir :: ItemBuilder a
    }

traverseTree :: ScaffoldTree -> Int -> DocBuilder (IO ()) -> IO ()
traverseTree tree level docBuilder = mapM_ (stepTree level docBuilder) tree


stepTree :: Int -> DocBuilder (IO ()) -> SfItem -> IO ()
stepTree level builder item = do
    case item of
        (SfFile name) -> (buildFile builder) item level
        (SfDir name subitems) -> (buildDir builder) item level
