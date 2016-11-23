module Magento ( getBaseDir
               , getModuleDir
               , getModuleNamespace
               , getModuleName
               , appDir
               , cacheDir
               , codeDir
               , designDir
               , etcDir
               , exportDir
               , logDir
               , mediaDir
               , sessionDir
               , skinDir
               , tmpDir
               , uploadDir
               ) where

import Data.List (elem, find, lookup)
import Data.List.Split (splitOn)
import Safe (atMay)
import System.FilePath.Posix

getBaseDir :: FilePath -> String -> Maybe FilePath
getBaseDir root "base" = Just root
getBaseDir root dir = (</> dir) <$> (lookup dir dirAssoc >>= getBaseDir root)
  where
    dirAssoc = [ ("app",    "base")
               , ("lib",    "base")
               , ("media",  "base")
               , ("skin",   "base")
               , ("var",    "base")
               , ("code",   "app")
               , ("design", "app")
               , ("etc",    "app")
               , ("locale", "app")
               , ("tmp",    "var")
               , ("cache",  "var")
               , ("log",    "var")
               , ("session","var")
               , ("export", "var")
               , ("upload", "media")
               ]

-- Takes a module name in the form Namespace_Module and what part of it to return
-- Returns Nothing in case of unexpected input
parseModuleString :: String -> Int -> Maybe String
parseModuleString s prt = (splitOn "_" s) `atMay` prt

getModuleNamespace :: String -> Maybe String
getModuleNamespace s = parseModuleString s 0

getModuleName :: String -> Maybe String
getModuleName s = parseModuleString s 1             

getModuleDir :: FilePath -> String -> String -> Maybe FilePath
getModuleDir root codepool module_ = do
  mdl <- getModuleName module_
  ns <- getModuleNamespace module_
  cp <- find (\x -> x == codepool) codepools
  (</> mdl) <$> (</> ns) <$> (</> cp) <$> getBaseDir root "code"
  where
    codepools = ["core", "community", "local"]

appDir :: FilePath -> Maybe FilePath
appDir root = getBaseDir root "app"

cacheDir :: FilePath -> Maybe FilePath
cacheDir root = getBaseDir root "cache"
              
codeDir :: FilePath -> Maybe FilePath
codeDir root = getBaseDir root "code"

designDir :: FilePath -> Maybe FilePath
designDir root = getBaseDir root "design"

etcDir :: FilePath -> Maybe FilePath
etcDir root = getBaseDir root "etc"

exportDir :: FilePath -> Maybe FilePath
exportDir root = getBaseDir root "export"

logDir :: FilePath -> Maybe FilePath
logDir root = getBaseDir root "log"

mediaDir :: FilePath -> Maybe FilePath
mediaDir root = getBaseDir root "media"

sessionDir :: FilePath -> Maybe FilePath
sessionDir root = getBaseDir root "session"

skinDir :: FilePath -> Maybe FilePath
skinDir root = getBaseDir root "skin"

tmpDir :: FilePath -> Maybe FilePath
tmpDir root = getBaseDir root "tmp"

uploadDir :: FilePath -> Maybe FilePath
uploadDir root = getBaseDir root "upload"
