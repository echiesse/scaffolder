module Main where


import System.Environment
import Scaffold
import Parser
import Utils

operations = [
        ("run", cmd_scaffold),
        ("pprint", cmd_prettyPrint),
        ("reverse", cmd_reverse)
    ]

cmd_scaffold args = withTextFile (head args) scaffold
cmd_prettyPrint args = withTextFile (head args) (pprint . parseDoc)
cmd_reverse args = putStrLn "Not implemented"

main = do
    allArgs <- getArgs
    case allArgs of
        [] -> printUsage
        (operation:args) -> do
            case lookup operation operations of
                Nothing -> putStrLn "Invalid operation"
                Just handler -> handler args

printUsage = putStrLn "Usage: scaffolder <operation> [args]"
