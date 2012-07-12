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

-- ----------------------------------------------------------------------------

module IndexTypes
    ( module IndexTypes
    , module Holumbus.Index.CompactIndex
    , Score
    )
where

import           CurryInfo

import           Holumbus.Index.CompactIndex
import           Holumbus.Query.Result          ( Score )

-- ------------------------------------------------------------

emptyCurryModState :: CurryModIndexerState
emptyCurryModState
    = emptyIndexerState emptyInverted emptyDocuments

emptyCurryFctState :: CurryFctIndexerState
emptyCurryFctState
    = emptyIndexerState emptyInverted emptyDocuments

emptyCurryTypeState :: CurryTypeIndexerState
emptyCurryTypeState
    = emptyIndexerState emptyInverted emptyDocuments

emptyCurryState :: HolumbusState a
emptyCurryState = emptyIndexerState emptyInverted emptyDocuments
-- ------------------------------------------------------------

type CurryModIndexerState        = HolumbusState   ModuleInfo
type CurryModIndexerConfig       = HolumbusConfig  ModuleInfo

type CurryFctIndexerState        = HolumbusState   FunctionInfo
type CurryFctIndexerConfig       = HolumbusConfig  FunctionInfo

type CurryTypeIndexerState       = HolumbusState   TypeInfo
type CurryTypeIndexerConfig      = HolumbusConfig  TypeInfo

type CurryIndexerState a         = CompactInverted -> SmallDocuments a
-- ------------------------------------------------------------

type ModuleIxDoc   = Inverted -> Documents ModuleInfo
type FunctionIxDoc = Inverted -> Documents FunctionInfo
type TypeIxDoc     = Inverted -> Documents TypeInfo
