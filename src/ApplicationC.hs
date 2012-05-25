{-# OPTIONS -XTypeSynonymInstances -XMultiParamTypeClasses #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Application

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This module defines our application's monad and any application-specific
  information it requires.

-}

-- ----------------------------------------------------------------------------

module ApplicationC
  ( Application
  , applicationInitializer
  )
where
import Snap.Extension
import Snap.Extension.Heist.Impl
import CurryState

------------------------------------------------------------------------------

-- 'Application' is our application's monad. It uses 'SnapExtend' from
-- 'Snap.Extension' to provide us with an extended 'MonadSnap' making use of
-- the Heist extension.
type Application = SnapExtend ApplicationState

------------------------------------------------------------------------------

-- 'ApplicationState' is a record which contains the state needed by the Snap
-- extension we're using.  We're using Heist so we can easily render Heist
-- templates.

data ApplicationState = ApplicationState
  {
    templateState :: HeistState Application,
    curryState      :: CurryState
  }

------------------------------------------------------------------------------

instance HasHeistState Application ApplicationState where
  getHeistState     = templateState
  setHeistState s a = a { templateState = s }


instance HasCurryState ApplicationState where
  getCurryState     = curryState
  setCurryState s a = a { curryState = s }

------------------------------------------------------------------------------

-- The 'Initializer' for ApplicationState. This is used to generate the
-- 'ApplicationState' needed for our application and will automatically
-- generate reload\/cleanup actions for us.

applicationInitializer :: Initializer ApplicationState
applicationInitializer = do
    heist <- heistInitializer "resources/templates"
    curry <- curryInitializer
    return $ ApplicationState heist curry

------------------------------------------------------------------------------
