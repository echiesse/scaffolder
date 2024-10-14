module Commands where

import Data.List (sort)
import System.Directory
import System.Environment
import System.FilePath

import qualified Config
import FileSystem
import Parser
import Scaffold
import Utils
import ProgramInfo
import System.Exit

type Command = [String] -> IO ()

commands = [
        ("run", cmdScaffold),
        ("pprint", cmdPrettyPrint),
        ("reverse", cmdReverse),
        ("register", cmdRegister),
        ("list-templates", cmdListTemplates)
    ]

cmdScaffold :: Command
cmdScaffold args = do
    let templateName = head args
    maybeTemplatePath <- findTemplate templateName
    case maybeTemplatePath of
        Nothing -> do
            putStrLn $ "Template not found " ++ templateName
            exitWith (ExitFailure 1)
        Just templatePath -> do
            let baseDir =
                    case tail args of
                        [] -> Nothing
                        (path:_) -> Just path
            maybeEnsureDir baseDir
            withTextFile templatePath (scaffold baseDir)

cmdPrettyPrint :: Command
cmdPrettyPrint args = withTextFile (head args) (pprint . parseDoc)

cmdReverse :: Command
cmdReverse args = do
    case args of
        (baseDir: _) -> undefined
        -- (baseDir: _) -> do
        --     let name = takeBaseName $ dropTrailingPathSeparator baseDir
        --     tree <- traverseDir baseDir

        _ -> undefined


-- Template commands:

cmdRegister :: Command
cmdRegister args = do
    case args of
        (templateName: templatePath: _) -> do
            -- Verificar se há template registrado com o nome atual
                -- Se houver, confirmar substituição ou exigir que primeiro o existente seja apagado
                -- Não havendo template,
                    -- Validar template sendo importado
                    -- Salvar template no repositório
            templateDestPath <- getTemplatePath templateName
            copyFile templatePath templateDestPath
        _ -> error "Not enough input data"


cmdListTemplates :: Command
cmdListTemplates args = do
    files <- getTemplatesDirPath >>= listDirectory
    printList $ sort files
