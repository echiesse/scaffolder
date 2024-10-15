module TemplateManager where

import System.Directory
import ProgramInfo


doesTemplateExist :: String -> IO Bool
doesTemplateExist templateName = do
    templateDestPath <- getTemplatePath templateName
    doesFileExist templateDestPath


registerTemplate templateName templatePath = do
    templateDestPath <- getTemplatePath templateName
    copyFile templatePath templateDestPath