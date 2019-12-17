module Commands where

import Parser
import System.Environment
import Scaffold
import Utils

type Command = [String] -> IO ()

commands = [
        ("run", cmd_scaffold),
        ("pprint", cmd_prettyPrint),
        ("reverse", cmd_reverse)
    ]

cmd_scaffold :: Command
cmd_scaffold args = do
    let inputFilePath = head args
    let baseDir =
            case tail args of
                [] -> Nothing
                (path:_) -> Just path
    ensureDir baseDir
    withTextFile inputFilePath (scaffold baseDir)

cmd_prettyPrint :: Command
cmd_prettyPrint args = withTextFile (head args) (pprint . parseDoc)

cmd_reverse :: Command
cmd_reverse args = putStrLn "Not implemented"
