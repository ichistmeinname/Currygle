{- |
Module      :  Application
Description :  Defines the application's monad and related informations
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

Defines the application's monad, provides HasState instances and an initializer function to construct the AppState.
-}


module Application
  ( Application
  , appInitializer
  )
where
import Snap.Extension
import Snap.Extension.Heist.Impl
import CurryState

-- | Constructs an ApplicationState.
appInitializer :: Initializer ApplicationState
appInitializer = do
  heistS <- heistInitializer "resources/templates"
  curryS <- curryInitializer
  return $ ApplicationState heistS curryS

-- | Instance to find the heist state.
instance HasHeistState Application ApplicationState where
  getHeistState     = heistState
  setHeistState s a = a { heistState = s }

-- | Instance to find the curry state.
instance HasCurryState ApplicationState where
  getCurryState     = curryState
  setCurryState s a = a { curryState = s }

-- | Record type for holding application's state,
-- this includes the state needed by the used extensions.
data ApplicationState = ApplicationState
  {
    heistState :: HeistState Application,
    curryState :: CurryState
  }

type Application = SnapExtend ApplicationState
