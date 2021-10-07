{-# LANGUAGE FlexibleInstances #-}

module ScaffoldBuilder where

import ScaffoldTree

type ItemBuilder a = SfItem -> Int -> a
data ScaffoldBuilder a = ScaffoldBuilder {
        buildFile :: ItemBuilder a,
        buildDir :: ItemBuilder a
    }


class Builder a where
    traverseTree :: ScaffoldTree -> Int -> ScaffoldBuilder a -> a -> a
    stepTree :: SfItem -> Int -> ScaffoldBuilder a -> a -> a


instance Builder (IO a) where
    traverseTree [] level builder previousValue = previousValue
    traverseTree (item:items) level builder previousValue = do
        stepTree level builder item
        traverseTree items level builder previousValue

    stepTree level builder item = do
        case item of
            (SfFile name) -> (buildFile builder) item level
            (SfDir name subitems) -> (buildDir builder) item level


instance {-# OVERLAPPING #-} Builder String where
    traverseTree [] level builder result = result
    traverseTree (item:items) level builder result = traverseTree items level builder newResult
        where
            newResult = stepTree item level builder result

    stepTree item level builder result = do
        case item of
            (SfFile name) -> (buildFile builder) item level
            (SfDir name subitems) -> (buildDir builder) item level
