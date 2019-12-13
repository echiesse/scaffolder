module SfAST where

data Token = Indent | Word String

type SfAST = [SfItem]

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
