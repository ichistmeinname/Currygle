-- ----------------------------------------------------------------------------

{- |
  Module     : CoreData

  Author     : Sandra Dylus
  Version    : 0.1

  Maintains the definition of the data Core, that represents the index and
  documents. 
-}

-- ----------------------------------------------------------------------------

module CoreData
    ( Core(..)
    )
where

import IndexTypes
import CurryInfo

-- ------------------------------------------------------------
-- | datatype representing the index data

data Core = Core
          { index     :: ! CompactInverted
          , documents :: ! (SmallDocuments CurryInfo)
          }

-- ------------------------------------------------------------




