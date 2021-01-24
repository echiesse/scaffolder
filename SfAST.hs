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
        sfdocFolderName :: String,
        sfdocFolderContents :: [SfItem]
    }
    | SfFile {sfdocFileName :: String}

    deriving (Eq)

instance Show SfItem where
    show (SfFile name) = "File: " ++ name
    show (SfDir name doc) = "Dir: " ++ name ++ show doc
