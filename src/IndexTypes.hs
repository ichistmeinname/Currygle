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
  , IndexerState (..)
  , ModuleInfo
  , FunctionInfo
  , TypeInfo
 ) where

import CurryInfo

import Holumbus.Index.CompactIndex
import Holumbus.Crawler.IndexerCore (IndexerState (..))

-- | Triple of the index-documents-pairs
type CurryIndexerStates =
  ( -- | Pair of index and documents of the type ModuleInfo
    HolumbusState ModuleInfo
  , -- | Pair of index and documents of the type FunctionInfo
    HolumbusState FunctionInfo
  , -- | Pair of index and documents of the type TypeInfo
    HolumbusState TypeInfo
  )

emptyCurryIndexerStates :: CurryIndexerStates
emptyCurryIndexerStates = (emptyState, emptyState, emptyState)
  where emptyState = emptyIndexerState emptyInverted emptyDocuments

makeIndexerState :: Inverted -> Documents a -> HolumbusState a
makeIndexerState = IndexerState

-- ------------------------------------------------------------

-- | Pair of the loaded index and polomorph documents
type LoadedIndexerState a  = (CompactInverted, SmallDocuments a)

-- | Tripe of the loaded index-documents=pairs
type LoadedIndexerStates = ( LoadedIndexerState ModuleInfo
                           , LoadedIndexerState FunctionInfo
                           , LoadedIndexerState TypeInfo
                           )
