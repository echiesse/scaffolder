{-|
Module      : ScaffoldTree
Description : Provides the types to represent the abstract syntax tree (AST) of
    the directory structure.
-}
module ScaffoldTree where

data Token = Indent | Word String

type ScaffoldTree = [SfItem]

data SfItem =
    SfDir {
        sfDirName :: String,
        sfDirContents :: [SfItem]
    }
    | SfFile {sfFileName :: String}

    deriving (Eq)

instance Show SfItem where
    show (SfFile name) = "File: " ++ name
    show (SfDir name doc) = "Dir: " ++ name ++ show doc
