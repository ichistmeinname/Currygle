{- |
Module      :  FilesAndLoading
Description :  Helper module
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

This module defines helper functions relating to loading data or file names.
-}

module FilesAndLoading where

import System.FilePath

-- Extension for the temporary index/doc/list file
_tempFile :: String
_tempFile = "2"

_currydocExtension :: String
_currydocExtension = ".cdoc"

-- List file extension
_listFileExtension :: String
_listFileExtension = ".list"

-- Index file extension
_indexFileExtension :: String
_indexFileExtension = ".idx"

-- Document file extension
_documentFileExtension :: String
_documentFileExtension = ".doc"

_moduleIndexPath :: FilePath
_moduleIndexPath = "./index/ix-mod.bin"

_functionIndexPath :: FilePath
_functionIndexPath = "./index/ix-fct.bin"

_typeIndexPath :: FilePath
_typeIndexPath = "./index/ix-type.bin"

_moduleListPath :: FilePath
_moduleListPath = "./index/module"

pidFile :: String
pidFile = "server.pid"

-- Alias to read a given CurryInfo data file
readFromFile :: Read a => FilePath -> IO a
readFromFile path = readFile path >>= readIO

-- -- Alias to load index with explicit type
-- loadIndex :: FilePath -> IO CompactInverted
-- loadIndex = loadFromFile
--
-- -- Alias to load documents with explicit type
-- loadDocuments :: Binary a => FilePath -> IO (SmallDocuments a)
-- loadDocuments = loadFromBinFile

hasExtension :: String -> FilePath -> Bool
hasExtension ext fp = takeExtension fp == ext

-- Shortcut for index file extension
indexExtension :: FilePath -> FilePath
indexExtension = (<.> _indexFileExtension)

-- Shortcut for document file extension
documentExtension :: FilePath -> FilePath
documentExtension = (<.> _documentFileExtension)

-- Shortcut for list file extension
listExtension :: FilePath -> FilePath
listExtension = (<.> _listFileExtension)
