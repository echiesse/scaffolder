module ProgramInfo where

import System.Directory
import System.FilePath

import qualified Config


getAppDataPath :: IO FilePath
getAppDataPath = do
    homeDir <- getHomeDirectory
    return $ joinPath [homeDir, Config.dataDirName]

getPathFromAppPath :: FilePath -> IO FilePath
getPathFromAppPath path = do
    appPath <- getAppDataPath
    return $ joinPath [appPath, path]


getTemplatesDirPath :: IO FilePath
getTemplatesDirPath = getPathFromAppPath Config.templatesSubdir


getTemplatePath :: String -> IO FilePath
getTemplatePath templateName = do
    templateDirPath <- getTemplatesDirPath
    return $ joinPath [templateDirPath, templateName]


findTemplate :: String -> IO (Maybe FilePath)
findTemplate templateName = do
    path <- getTemplatePath templateName
    fileExists <- doesFileExist path
    return $ if fileExists
        then  Just path
        else  Nothing
