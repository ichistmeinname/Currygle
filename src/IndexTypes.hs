{-# OPTIONS #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : IndexTypes

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable

  The List of sub indexes created in the index file
-}

-- ----------------------------------------------------------------------------

module IndexTypes
    ( module IndexTypes
    , module Holumbus.Index.CompactIndex
    -- , CurryInfo(..)
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

type CurryTypeIndexerState        = HolumbusState   TypeInfo
type CurryTypeIndexerConfig       = HolumbusConfig  TypeInfo
-- ------------------------------------------------------------

