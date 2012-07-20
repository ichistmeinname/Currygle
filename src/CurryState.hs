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
  ( CurryState(..)
  , HasCurryState(..)
  , MonadCurry(..)
  , curryInitializer
  ) where

import Control.Monad.Reader
import Holumbus.Index.Common

import Snap.Extension
import Snap.Types

import System.IO (stderr, hPutStrLn)

import CoreData
import FilesAndLoading

-- Helper function to load the three pairs of index and documents and return it as Core data.
curryInitState :: IO Core
curryInitState = do
  idxMod  <- loadIndex curryModIndex
  infoMsg "index" (sizeWords idxMod) "words"
  docMod  <- loadDocuments curryModDocs
  infoMsg "documents" (sizeDocs docMod) "entries"
  idxFct  <- loadIndex curryFctIndex
  infoMsg "index" (sizeWords idxFct) "words"
  docFct  <- loadDocuments curryFctDocs
  infoMsg "documents" (sizeDocs docFct) "entries"
  idxType <- loadIndex curryTypeIndex
  infoMsg "index" (sizeWords idxType) "words"
  docType <- loadDocuments curryTypeDocs
  infoMsg "documents" (sizeDocs docType) "entries"
  return Core { modIndex      = idxMod,
                modDocuments  = docMod,
                fctIndex      = idxFct,
                fctDocuments  = docFct,
                typeIndex     = idxType,
                typeDocuments = docType
              }
 where infoMsg str1 fIdxOrDoc str2 = 
         hPutStrLn stderr $ "Init process: Curry " 
                          ++ str1 ++ " was loaded successfully and contains "
                          ++ show fIdxOrDoc ++ " " ++ str2

-- | Initializes the 'CurryState'.
curryInitializer :: Initializer CurryState
curryInitializer = liftIO curryInitState  >>= mkInitializer . CurryState

instance InitializerState CurryState where
    extensionId = const "Curry/CurryState"
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()

class HasCurryState s where
    getCurryState :: s -> CurryState
    setCurryState :: CurryState -> s -> s

instance HasCurryState s => MonadCurry (SnapExtend s) where
    curryCore = fmap core $ asks getCurryState

instance (MonadSnap m, HasCurryState s) => MonadCurry (ReaderT s m) where
    curryCore = fmap core $ asks getCurryState

class MonadSnap m => MonadCurry m where
    curryCore :: m Core

newtype CurryState
    = CurryState { core :: Core }


