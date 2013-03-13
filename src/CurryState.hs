{-# LANGUAGE OverloadedStrings #-}

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
module CurryState
  ( CurryState (..)
  , HasCurryState (..)
  , curryInitializer
  ) where

import Control.Monad.Trans   (liftIO)
import System.IO             (stderr, hPutStrLn)
import Holumbus.Index.Common (sizeWords, sizeDocs)

import Snap.Extension (Initializer, InitializerState (..), mkInitializer)

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

instance InitializerState CurryState where
  extensionId = const "Curry/CurryState"
  mkCleanup   = const $ return ()
  mkReload    = const $ return ()

class HasCurryState s where
  getCurryState :: s -> CurryState
  setCurryState :: CurryState -> s -> s

-- | Initializes the 'CurryState'.
curryInitializer :: Initializer CurryState
curryInitializer = liftIO curryInitState >>= mkInitializer

-- Helper function to load the three pairs of index and documents
-- and return it as Core data.
curryInitState :: IO CurryState
curryInitState = do
  (idxMod, docMod) <- loadIndexDocs _curryModIndex  _curryModDocs
  (idxFct, docFct) <- loadIndexDocs _curryFctIndex  _curryFctDocs
  (idxTyp, docTyp) <- loadIndexDocs _curryTypeIndex _curryTypeDocs
  return CurryState
    { modIndex  = idxMod, modDocuments  = docMod
    , fctIndex  = idxFct, fctDocuments  = docFct
    , typeIndex = idxTyp, typeDocuments = docTyp
    }
  where
  loadIndexDocs i d = do
    idx <- loadIndex i
    info "index" (sizeWords idx) "words"
    doc <- loadDocuments d
    info "documents" (sizeDocs doc) "entries"
    return (idx, doc)
  info what count cntnt = hPutStrLn stderr $ unwords
    ["Init process: Curry", what, "was loaded", '(':show count, cntnt ++ ")"]
