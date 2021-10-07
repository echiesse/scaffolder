module Commands where

import System.Directory
import System.Environment
import System.FilePath

import qualified Config
import FileSystem
import Parser
import Scaffold
import Utils

type Command = [String] -> IO ()

commands = [
        ("run", cmd_scaffold),
        ("pprint", cmd_prettyPrint),
        ("reverse", cmd_reverse),
        ("register", cmd_register)
    ]

cmd_scaffold :: Command
cmd_scaffold args = do
    let inputFilePath = head args
    let baseDir =
            case tail args of
                [] -> Nothing
                (path:_) -> Just path
    maybeEnsureDir baseDir
    withTextFile inputFilePath (scaffold baseDir)

cmd_prettyPrint :: Command
cmd_prettyPrint args = withTextFile (head args) (pprint . parseDoc)

cmd_reverse :: Command
cmd_reverse args = undefined
-- cmd_reverse args = do
--    case args of
--        (baseDir: _) -> do
--            let name = takeBaseName $ dropTrailingPathSeparator baseDir
--            tree <- traverseDir baseDir
--
--        _ -> undefined


cmd_register :: Command
cmd_register args = do
    case args of
        (templateName: templatePath: _) -> do
            -- Verificar se há template registrado com o nome atual
                -- Se houver, confirmar substituição ou exigir que primeiro o existente seja apagado
                -- Não havendo template,
                    -- Validar template sendo importado
                    -- Salvar template no repositório
            scaffolderDirPath <- ensureScaffolderDir
            copyFile templatePath $ joinPath [scaffolderDirPath, Config.templatesSubdir, templateName]
        _ -> error "Not enough input data"
