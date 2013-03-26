{- |
Module      :  IndexTypes
Description :  Defines indexer states
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

This module define the indexer types that are used by the search engine.
-}
module IndexTypes
  ( module IndexTypes
  , module Holumbus.Index.CompactIndex
  , ModuleInfo
  , FunctionInfo
  , TypeInfo
  ) where

import Control.Monad               (when)
import System.IO                   (stderr, hPutStrLn)

import Holumbus.Index.Common (loadFromFile, loadFromBinFile)
import Holumbus.Index.Common       (sizeWords, sizeDocs)
import Holumbus.Index.CompactIndex

import CurryInfo
import FilesAndLoading

data CurryIndex = CurryIndex
  { modIdx  :: ! CompactInverted
  , modDocs :: ! (SmallDocuments ModuleInfo)
  , fctIdx  :: ! CompactInverted
  , fctDocs :: ! (SmallDocuments FunctionInfo)
  , typIdx  :: ! CompactInverted
  , typDocs :: ! (SmallDocuments TypeInfo)
  }

-- Helper function to load the three pairs of index and documents
-- and return it as Core data.
loadCurryIndex :: Bool -> IO CurryIndex
loadCurryIndex verbose = do
  (mi, md) <- loadIdxDoc "modules"   _moduleIndexPath
  (fi, fd) <- loadIdxDoc "functions" _functionIndexPath
  (ti, td) <- loadIdxDoc "types"     _typeIndexPath
  return $ CurryIndex mi md fi fd ti td
  where
  loadIdxDoc what path = do
    idx <- loadFromFile    (indexExtension    path)
    doc <- loadFromBinFile (documentExtension path)
    when verbose $ info what (sizeWords idx) (sizeDocs doc)
    return (idx, doc)
  info what wCnt dCnt = hPutStrLn stderr $ unwords
    [ "Init process:", "Index for", what, "loaded", '(' : show wCnt, "words,"
    , show dCnt, "documents)"]
