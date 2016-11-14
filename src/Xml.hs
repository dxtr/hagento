module Xml (getXmlFiles) where

import Conduit
--import Data.Conduit
--import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
--import qualified Data.Text as T

import System.FilePath.Posix

--import Args (Options(..))

isXmlFile :: FilePath -> Bool
isXmlFile f = takeExtension f == ".xml"
--isXmlFile f = T.takeEnd 4 (T.pack f) == (T.pack ".xml")

-- Get a list of all XML files in app/
getXmlFiles :: String -> IO [FilePath]
getXmlFiles r = runResourceT (sourceDirectoryDeep True r $$ CC.filter isXmlFile .| sinkList)

-- listXmlFiles magentoRoot =
--   runResourceT (sourceDirectoryDeep True actualRoot $$ CC.filter isXmlFile =$ CC.mapM_ putStrLn)
--   where
--     actualRoot = magentoRoot ++ "/app/"
  
