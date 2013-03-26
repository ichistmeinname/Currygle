{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Application
Description :  Defines the application's monad and related informations
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

Defines the application's monad, provides HasState instances
and an initializer function to construct the AppState.
-}
module Application ( Application, appInitializer, HasCurryIndex (..) ) where

import Control.Monad.Trans   (liftIO)

import Snap.Extension
import Snap.Extension.Heist.Impl

import IndexTypes ( CurryIndex (..), loadCurryIndex )

type Application = SnapExtend ApplicationState

-- | Record type for holding application's state,
-- this includes the state needed by the used extensions.
data ApplicationState = ApplicationState
  { heistState :: HeistState Application
  , curryState :: CurryIndex
  }

-- | Constructs an ApplicationState.
appInitializer :: Initializer ApplicationState
appInitializer = do
  heistS <- heistInitializer "resources/templates" id
  curryS <- curryInitializer True
  return $ ApplicationState heistS curryS

-- | Initializes the 'CurryState'.
curryInitializer :: Bool -> Initializer CurryIndex
curryInitializer verbose = liftIO (loadCurryIndex verbose) >>= mkInitializer

instance InitializerState CurryIndex where
  extensionId = const "Curry/CurryIndex"
  mkCleanup   = const $ return ()
  mkReload    = const $ return ()

class HasCurryIndex s where
  getCurryIndex :: s -> CurryIndex

-- | Instance to find the heist state.
instance HasHeistState Application ApplicationState where
  getHeistState     = heistState
  setHeistState s a = a { heistState = s }

-- | Instance to find the curry state.
instance HasCurryIndex ApplicationState where
  getCurryIndex = curryState
