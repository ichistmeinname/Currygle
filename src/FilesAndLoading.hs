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

import Data.Binary (Binary (..))
import IndexTypes
import CurryInfo
import Holumbus.Index.Common (loadFromFile, loadFromBinFile)

-- Alias to load index with explicit type
loadIndex :: FilePath -> IO CompactInverted
loadIndex = loadFromFile

loadDocuments :: (Binary a) => FilePath -> IO (SmallDocuments a)
loadDocuments = loadFromBinFile

-- Alias to load documents with explicit type 
loadModDocuments   :: FilePath -> IO (SmallDocuments ModuleInfo)
loadModDocuments   = loadFromBinFile

loadFctDocuments   :: FilePath -> IO (SmallDocuments FunctionInfo)
loadFctDocuments   = loadFromBinFile

loadTypeDocuments   :: FilePath -> IO (SmallDocuments TypeInfo)
loadTypeDocuments   = loadFromBinFile

-- add file extension
addFileExtension :: FilePath -> FilePath -> FilePath
addFileExtension extension fileName = fileName ++ extension

indexExtension :: FilePath -> FilePath 
indexExtension = addFileExtension indexFileExtension

documentExtension :: FilePath -> FilePath
documentExtension = addFileExtension documentFileExtension

moduleIndexPath :: FilePath
moduleIndexPath = "./index/ix-mod.bin"
-- moduleIndex2 :: FilePath
-- moduleIndex2 = "./index/ix-mod.bin2"

functionIndexPath :: FilePath
functionIndexPath = "./index/ix-fct.bin"
-- functionIndex2 :: FilePath
-- functionIndex2 = "./index/ix-fct.bin2"

typeIndexPath :: FilePath
typeIndexPath = "./index/ix-type.bin"
-- typeIndex2 :: FilePath
-- typeIndex2 = "./index/ix-type.bin2""

-- extension for the temporary index/doc file
tempFile :: String
tempFile = "2"

-- filePath :: FilePath
-- filePath = "../resources/static"

-- index file extension
indexFileExtension :: FilePath
indexFileExtension = ".idx"

-- document file extension
documentFileExtension :: FilePath
documentFileExtension = ".doc"

-- paths to load index and document files
curryModIndex :: FilePath
curryModIndex  = "./index/ix-mod.bin.idx"
curryModDocs :: FilePath 
curryModDocs   = "./index/ix-mod.bin.doc"
curryFctIndex :: FilePath
curryFctIndex  = "./index/ix-fct.bin.idx"
curryFctDocs :: FilePath
curryFctDocs   = "./index/ix-fct.bin.doc"
curryTypeIndex :: FilePath
curryTypeIndex = "./index/ix-type.bin.idx"
curryTypeDocs :: FilePath
curryTypeDocs  = "./index/ix-type.bin.doc"
