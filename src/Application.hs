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
module Application ( Application, appInitializer, HasCurryState (..) ) where

import Control.Monad.Trans   (liftIO)

import Snap.Extension
import Snap.Extension.Heist.Impl

import CurryState ( CurryState (..), loadCurryState )

type Application = SnapExtend ApplicationState

-- | Record type for holding application's state,
-- this includes the state needed by the used extensions.
data ApplicationState = ApplicationState
  { heistState :: HeistState Application
  , curryState :: CurryState
  }

-- | Constructs an ApplicationState.
appInitializer :: Initializer ApplicationState
appInitializer = do
  heistS <- heistInitializer "resources/templates" id
  curryS <- curryInitializer
  return $ ApplicationState heistS curryS

-- | Initializes the 'CurryState'.
curryInitializer :: Initializer CurryState
curryInitializer = liftIO (loadCurryState True) >>= mkInitializer

instance InitializerState CurryState where
  extensionId = const "Curry/CurryState"
  mkCleanup   = const $ return ()
  mkReload    = const $ return ()

class HasCurryState s where
  getCurryState :: s -> CurryState
  setCurryState :: CurryState -> s -> s

-- | Instance to find the heist state.
instance HasHeistState Application ApplicationState where
  getHeistState     = heistState
  setHeistState s a = a { heistState = s }

-- | Instance to find the curry state.
instance HasCurryState ApplicationState where
  getCurryState     = curryState
  setCurryState s a = a { curryState = s }
