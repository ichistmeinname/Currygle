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
