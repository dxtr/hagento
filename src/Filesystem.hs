{-# LANGUAGE OverloadedStrings #-}

module Filesystem ( isXmlFile
                  , isDotFile
                  , isDirectory
                  , isRegularFile
                  , isSymbolicLink
                  , isBlockDevice
                  , isCharacterDevice
                  , isNamedPipe
                  , isSocket
                  , isValidFile
                  , getAllFilesInDir
                  , getAllFilesOfType
                  , chdir
) where

import Data.ByteString.Char8 (ByteString, pack, unpack)
import HPath
import HPath.IO
import System.Posix.ByteString.FilePath
import System.Posix.Directory.ByteString
import System.Posix.Directory.Traversals
import System.Posix.FilePath
--import System.Posix.Files hiding (isSymbolicLink, isRegularFile)
import qualified System.Posix.Files as F

chdir :: RawFilePath -> IO ()
chdir = changeWorkingDirectory

cmpFileType :: RawFilePath -> FileType -> IO Bool
cmpFileType fp ft = do
  f <- parseAbs fp
  fmap (ft ==) (getFileType f)

isXmlFile :: RawFilePath -> Bool
isXmlFile fp = hasExtension fp && takeExtension fp == extension
  where
    extension = pack ".xml"
    
isDotFile :: RawFilePath -> IO Bool
isDotFile fp = return $ hiddenFile fp

isDirectory :: RawFilePath -> IO Bool
isDirectory fp = cmpFileType fp Directory

isRegularFile :: RawFilePath -> IO Bool
isRegularFile fp = cmpFileType fp RegularFile

isSymbolicLink :: RawFilePath -> IO Bool
isSymbolicLink fp = cmpFileType fp SymbolicLink

isBlockDevice :: RawFilePath -> IO Bool
isBlockDevice fp = cmpFileType fp BlockDevice

isCharacterDevice :: RawFilePath -> IO Bool
isCharacterDevice fp = cmpFileType fp CharacterDevice

isNamedPipe :: RawFilePath -> IO Bool
isNamedPipe fp = cmpFileType fp NamedPipe

isSocket :: RawFilePath -> IO Bool
isSocket fp = cmpFileType fp Socket

isValidFile :: RawFilePath -> Bool -> IO Bool
isValidFile fp allowSymLink = do
  fileStatus <- F.getFileStatus (unpack fp)
  let regularFile = F.isRegularFile fileStatus
  let symbolicLink = F.isSymbolicLink fileStatus
  return (regularFile || (symbolicLink && allowSymLink))
  

getAllFilesInDir :: RawFilePath -> Bool -> IO [RawFilePath]
getAllFilesInDir fp followLinks = traverseDirectory flt [] fp
  where
    flt acc f = do
      valid <- isValidFile f followLinks
      if valid then return (f:acc)
      else return (acc)

getAllFilesOfType :: RawFilePath -> ByteString -> Bool -> IO [RawFilePath]
getAllFilesOfType fp typ followLinks = traverseDirectory flt [] fp
  where
    flt acc f = do
      valid <- isValidFile f followLinks
      let validType = hasExtension f && takeExtension f == typ
      if valid && validType then return (f:acc)
      else return (acc)
      
