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
-- | represents the index data, it differs between module, fct and type information

data Core = Core
          { modIndex      :: ! CompactInverted
          , modDocuments  :: ! (SmallDocuments ModuleInfo)
          , fctIndex      :: ! CompactInverted
          , fctDocuments  :: ! (SmallDocuments FunctionInfo)
          , typeIndex     :: ! CompactInverted
          , typeDocuments :: ! (SmallDocuments TypeInfo)
          }

-- ------------------------------------------------------------




