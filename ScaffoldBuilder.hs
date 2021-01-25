module ScaffoldBuilder where

import ScaffoldTree

type ItemBuilder a = SfItem -> Int -> a
data ScaffoldBuilder a = ScaffoldBuilder {
        buildFile :: ItemBuilder a,
        buildDir :: ItemBuilder a
    }

traverseTree :: ScaffoldTree -> Int -> ScaffoldBuilder (IO ()) -> IO ()
traverseTree tree level builder = mapM_ (stepTree level builder) tree


stepTree :: Int -> ScaffoldBuilder (IO ()) -> SfItem -> IO ()
stepTree level builder item = do
    case item of
        (SfFile name) -> (buildFile builder) item level
        (SfDir name subitems) -> (buildDir builder) item level
