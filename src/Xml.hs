{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Xml ( readXmlFile
           , readXmlFileList
           , readXmlFileMap
           , writeXmlFile
           , isXmlFile
           , getXmlFiles
           , getXmlPath
           , getXmlPathCursor
           , getCursorContent
           , textToXmlName
           , byteStringToXmlName
           , XML.Document(..)
           ) where

import Control.Concurrent.Async

--import Conduit
--import qualified Data.Conduit.Combinators as CC
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Text (Text, splitOn)
import qualified Data.Text as T (pack)
import Data.ByteString.Char8 (ByteString, unpack)
import qualified Text.XML as XML
import Text.XML.Cursor
--import qualified Text.Hamlet.XML as Hamlet

import Filesystem

--import Args (Options(..))
type Document = XML.Document

textToXmlName :: Text -> XML.Name
textToXmlName txt = XML.Name { XML.nameLocalName = txt
                             , XML.nameNamespace = Nothing
                             , XML.namePrefix = Nothing
                             }

byteStringToXmlName :: ByteString -> XML.Name
byteStringToXmlName bs = XML.Name { XML.nameLocalName = T.pack $ unpack bs
                                  , XML.nameNamespace = Nothing
                                  , XML.namePrefix = Nothing
                                  }

getXmlFiles :: ByteString -> IO [ByteString]
getXmlFiles path = getAllFilesOfType path ".xml" True

-- TODO: Catch exceptions
readXmlFile :: ByteString -> IO Document
readXmlFile src = XML.readFile XML.def $ unpack src

readXmlFileList :: [ByteString] -> IO [(ByteString, Document)]
readXmlFileList src = mapConcurrently doRead $ [(s, Nothing) | s <-  src]
  where
    doRead (fp,_) = do
      r <- readXmlFile fp
      return (fp, r)

readXmlFileMap :: [ByteString] -> IO (Map.Map ByteString Document)
readXmlFileMap src = do
  flist <- readXmlFileList src
  return $ Map.fromList flist

writeXmlFile :: FilePath -> Document -> IO ()
writeXmlFile dst output = XML.writeFile XML.def dst output  

getXmlPathCursor :: Document -> Text -> [Cursor]
getXmlPathCursor doc path = getXmlNode root $ List.map textToXmlName $ splitOn "/" path
  where
    root = fromDocument doc


getXmlPath :: Document -> Text -> [XML.Node]
getXmlPath doc path = List.map node $ getXmlPathCursor doc path

getCursorContent :: [Cursor] -> [Text]
getCursorContent cur = concat $ List.map (\x -> x $// content) cur

doGetXmlNode :: [Cursor] -> [XML.Name] -> [Cursor]
doGetXmlNode [] _ = []
doGetXmlNode c [] = c
doGetXmlNode (cur:tailcur) path@(x:xs) =
  (doGetXmlNode (cur $/ element x) xs) ++ (doGetXmlNode tailcur path)

getXmlNode :: Cursor -> [XML.Name] -> [Cursor]
getXmlNode n [] = [n]
getXmlNode n (x:xs) = doGetXmlNode (n $/ element x) xs
