module Commands where

import Data.List (sort)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

import qualified Config
import DirTraverser
import FileSystem
import Parser
import ProgramInfo
import Scaffold
import TemplateManager
import Utils

type Command = [String] -> IO ()

commands = [
        ("run", cmdScaffold),
        ("reverse", cmdReverse),
        ("template-list", cmdListTemplates),
        ("template-pprint", cmdTemplatePrettyPrint),
        ("template-register", cmdRegister),
        ("template-show", cmdTemplateShow),
        ("template-unregister", cmdTemplateUnregister)
    ]

cmdScaffold :: Command
cmdScaffold args = do
    let templateName = head args
    maybeTemplatePath <- findTemplate templateName
    case maybeTemplatePath of
        Nothing -> die $ "Template not found " ++ templateName
        Just templatePath -> do
            let baseDir =
                    case tail args of
                        [] -> Nothing
                        (path:_) -> Just path
            maybeEnsureDir baseDir
            withTextFile templatePath (scaffold baseDir)


cmdReverse :: Command
cmdReverse args = do
    case args of
        (baseDir: _) -> do
            tree <- traverseDir baseDir
            pprint tree

        _ -> die "Usage: scaffolder reverse <tree-path>"


-- Template commands:

cmdRegister :: Command
cmdRegister args = do
    case args of
        (templateName: templatePath: _) -> do
            templateExists <- doesTemplateExist templateName
            if templateExists
                then die $ "Template named \"" ++ templateName ++ "\" already exists"
                else registerTemplate templateName templatePath

        _ -> die "Usage: scaffolder register <template-name> <template-file-path>"


cmdTemplateUnregister :: Command
cmdTemplateUnregister args = do
    case args of
        (templateName: _) -> do
            templatePath <- getTemplatePath templateName
            templateExists <- doesFileExist templatePath
            if templateExists
                then do
                    removeFile templatePath
                    putStrLn $ "Template removed: " ++ templateName
                else putStrLn $ "Template not found: " ++ templateName
        _ -> die "Missing template name. Try:\nscaffolder template-unregister <template-name>"


cmdListTemplates :: Command
cmdListTemplates args = do
    files <- getTemplatesDirPath >>= listDirectory
    printList $ sort files


cmdTemplatePrettyPrint :: Command
cmdTemplatePrettyPrint args = do
    templatePath <- getTemplatePath $ head args
    withTextFile templatePath (pprint . parseDoc)


cmdTemplateShow :: Command
cmdTemplateShow args = do
    templatePath <- getTemplatePath $ head args
    withTextFile templatePath putStrLn
