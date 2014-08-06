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
 ( module IndexTypes,
   module Holumbus.Index.CompactIndex,
   IndexerState (..),
   ModuleInfo,
   FunctionInfo,
   TypeInfo
 )
where

import CurryInfo

import Holumbus.Index.CompactIndex
import Holumbus.Crawler.IndexerCore (IndexerState (..))

emptyCurryModState :: CurryModIndexerState
emptyCurryModState = emptyIndexerState emptyInverted emptyDocuments

emptyCurryFctState :: CurryFctIndexerState
emptyCurryFctState = emptyIndexerState emptyInverted emptyDocuments

emptyCurryTypeState :: CurryTypeIndexerState
emptyCurryTypeState = emptyIndexerState emptyInverted emptyDocuments

emptyCurryState :: HolumbusState a
emptyCurryState = emptyIndexerState emptyInverted emptyDocuments

makeIndexerState :: Inverted -> Documents a -> HolumbusState a
makeIndexerState = IndexerState
-- ------------------------------------------------------------

-- | Pair of index and documents of the type ModuleInfo
type CurryModIndexerState         = HolumbusState ModuleInfo

-- | Pair of index and documents of the type FunctionInfo
type CurryFctIndexerState         = HolumbusState FunctionInfo

-- | Pair of index and documents of the type TypeInfo
type CurryTypeIndexerState        = HolumbusState TypeInfo

-- | Pair of index and documents with polomorph type
type CurryIndexerState a          = HolumbusState a

-- | Triple of the index-documents-pairs
type CurryIndexerStates           = (CurryModIndexerState, 
                                    CurryFctIndexerState, 
                                    CurryTypeIndexerState)

-- | Pair of the loaded index and polomorph documents
type LoadedIndexerState a         = (CompactInverted, SmallDocuments a)

-- | Tripe of the loaded index-documents=pairs
type LoadedIndexerStates          = ((CompactInverted, SmallDocuments ModuleInfo),
                                    (CompactInverted, SmallDocuments FunctionInfo),
                                    (CompactInverted, SmallDocuments TypeInfo))
