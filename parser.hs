module Parser where

import Data.List
import Data.String.Utils

import ScaffoldTree

indentation = "    " -- <<<<< indentation não deve ser hardcoded
commentChar = '#'

removeComments :: [String] -> [String]
removeComments ls = map stripComments $ filter (not . isCommentedLine) ls

isCommentedLine line = strip line == "" || (head . strip) line == commentChar
stripComments = takeWhile (/= commentChar)

parseDoc :: String -> ScaffoldTree
parseDoc content = fst $ parseDocLines ((removeComments . lines) content) 0

parseDocLines :: [String] -> Int -> (ScaffoldTree, [String])
parseDocLines (line:ls) prevLevel
    | prevLevel == currentLevel || prevLevel < currentLevel =  -- Calcular novo item e apendar na lista de items
        case parseItem (line:ls) of
            Nothing -> parseDocLines ls prevLevel
            Just (item, remainingLines) -> ((item:items), lastLines)
                where
                    parsed = parseDocLines remainingLines currentLevel
                    items = fst parsed
                    lastLines = snd parsed
    | otherwise = ([], (line:ls))
    where
        currentLevel = countIndent indentation line
parseDocLines [] prevLevel = ([], [])

parseItem :: [String] -> Maybe (SfItem, [String])
parseItem (line:ls) =
    case (strip line) of
        ('-' : name) -> Just (SfFile $ strip name, ls)
        ('+' : name) -> Just (SfDir (strip name) (fst parsed), snd parsed)
        _ -> Nothing
    where
        parsed = case ls of
            [] -> ([], [])
            (l:lss) -> if countIndent indentation line < countIndent indentation l
                then parseDocLines ls (countIndent indentation line)
                else ([], ls)


countIndent :: String -> String -> Int
countIndent indentation text
    | indentation `isPrefixOf` text =
        1 + countIndent indentation (drop (length indentation) text)
    | otherwise = 0

detectIndent :: String -> Int
detectIndent = length .(takeWhile (==' ')). head . lines
