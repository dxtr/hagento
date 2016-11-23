{-# LANGUAGE OverloadedStrings #-}

module Module ( Module(..)
              , listModules
              ) where

import Data.Text (Text)
import Xml

data Codepool = Core | Community | Local deriving (Show)

data Module = Module { moduleCodePool :: Codepool
                     , moduleNameSpace :: String
                     , moduleName :: String
                     , moduleActive :: Bool
                     } deriving (Show)

data Config = Config {
                     } deriving (Show)

listModules :: Text -> IO ([Module])
listModules rootPath = do
  fileMap <- readXmlFileMap
