module Main where

import Commands
import Parser
import System.Environment
import Scaffold
import Utils


main = do
    allArgs <- getArgs
    case allArgs of
        [] -> printUsage
        (operation:args) -> do
            case lookup operation commands of
                Nothing -> putStrLn "Invalid operation"
                Just handler -> handler args

printUsage = putStrLn "Usage: scaffolder <operation> [args]"
