{- |
Module      :  CurryState
Description :  Defines a custom state
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

This module defines a custom state to store the index and documents.

-}
module CurryState ( CurryState (..), loadCurryState ) where

import Control.Monad         (when)
import System.IO             (stderr, hPutStrLn)

import Holumbus.Index.Common (loadFromFile, loadFromBinFile)
import Holumbus.Index.Common (sizeWords, sizeDocs)

import FilesAndLoading
import IndexTypes

data CurryState = CurryState
  { modIndex      :: ! CompactInverted
  , modDocuments  :: ! (SmallDocuments ModuleInfo)
  , fctIndex      :: ! CompactInverted
  , fctDocuments  :: ! (SmallDocuments FunctionInfo)
  , typeIndex     :: ! CompactInverted
  , typeDocuments :: ! (SmallDocuments TypeInfo)
  }

-- Helper function to load the three pairs of index and documents
-- and return it as Core data.
loadCurryState :: Bool -> IO CurryState
loadCurryState verbose = do
  (idxMod, docMod) <- loadIdxDoc "modules"   _moduleIndexPath
  (idxFct, docFct) <- loadIdxDoc "functions" _functionIndexPath
  (idxTyp, docTyp) <- loadIdxDoc "types"     _typeIndexPath
  return CurryState
    { modIndex  = idxMod, modDocuments  = docMod
    , fctIndex  = idxFct, fctDocuments  = docFct
    , typeIndex = idxTyp, typeDocuments = docTyp
    }
  where
  loadIdxDoc what path = do
    idx <- loadFromFile    (indexExtension    path)
    doc <- loadFromBinFile (documentExtension path)
    when verbose $ info what (sizeWords idx) (sizeDocs doc)
    return (idx, doc)
  info what wCnt dCnt = hPutStrLn stderr $ unwords
    [ "Init process:", "Index for", what, "loaded", '(' : show wCnt, "words,"
    , show dCnt, "documents)"]
