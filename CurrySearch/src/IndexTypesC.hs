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

module IndexTypesC
    ( module IndexTypesC
    , module Holumbus.Index.CompactIndex
    -- , CurryInfo(..)
    , Score
    )
where

import           CurryInfo

-- import           Holumbus.Crawler
import           Holumbus.Index.CompactIndex
import           Holumbus.Query.Result          ( Score )

-- ------------------------------------------------------------

emptyCurryState :: CurryIndexerState
emptyCurryState
    = emptyIndexerState emptyInverted emptyDocuments

-- ------------------------------------------------------------

type CurryIndexerState        = HolumbusState   CurryInfo
type CurryIndexerConfig       = HolumbusConfig  CurryInfo
-- type W3WIndexerCrawlerState = CrawlerState W3WIndexerState

-- ------------------------------------------------------------

