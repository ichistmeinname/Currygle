{- |
Module      :  CoreData
Description :  Representation of index and documents
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

Maintains the definition of the data Core, that represents the index and documents. 
-}

-- ----------------------------------------------------------------------------

module CoreData
    ( Core(..) )
where

import IndexTypes
import CurryInfo

-- ------------------------------------------------------------
-- | Represents the index data, it differs between module, fct and type information

data Core = Core
          { modIndex      :: ! CompactInverted
          , modDocuments  :: ! (SmallDocuments ModuleInfo)
          , fctIndex      :: ! CompactInverted
          , fctDocuments  :: ! (SmallDocuments FunctionInfo)
          , typeIndex     :: ! CompactInverted
          , typeDocuments :: ! (SmallDocuments TypeInfo)
          }

-- ------------------------------------------------------------




