{-# LANGUAGE OverloadedStrings #-}

-- ------------------------------------------------------------------------------
{-|

'Snap.Extension.Timer.Impl' is an implementation of the 'MonadTimer'
interface defined in 'Snap.Extension.Timer'.

As always, to use, add 'TimerState' to your application's state, along with an
instance of 'HasTimerState' for your application's state, making sure to use a
'timerInitializer' in your application's 'Initializer', and then you're ready to go.

This implementation does not require that your application's monad implement
interfaces from any other Snap Extension.

-}
-- ------------------------------------------------------------------------------

module CurryState
  ( CurryState(..)
  , HasCurryState(..)
  , MonadCurry(..)
  , curryInitializer
  ) where

import Control.Monad.Reader

import Holumbus.Index.Common

import Snap.Extension
import Snap.Types

import System.IO        ( stderr
                        , hPutStrLn
                        )

import CoreData
import CurrySearch

-- ------------------------------------------------------------------------------
-- | Your application's state must include a 'TimerState' in order for your
-- application to be a 'MonadTimer'.

newtype CurryState
    = CurryState
    { getCore :: Core
    }

-- ------------------------------------------------------------------------------
-- | For your application's monad to be a 'MonadTimer', your application's
-- state needs to be an instance of 'HasTimerState'. Minimal complete
-- definition: 'getTimerState', 'setTimerState'.

class HasCurryState s where
    getCurryState :: s -> CurryState
    setCurryState :: CurryState -> s -> s


-- ------------------------------------------------------------------------------
-- | The 'MonadCurry' type class. Minimal complete definition: 'curryCore'.

class MonadSnap m => MonadCurry m where
    -- | The core Curry state which was last loaded.
    curryCore :: m Core

-- ------------------------------------------------------------------------------

instance HasCurryState s => MonadCurry (SnapExtend s) where
    curryCore = fmap getCore $ asks getCurryState

-- ------------------------------------------------------------------------------

instance (MonadSnap m, HasCurryState s) => MonadCurry (ReaderT s m) where
    curryCore = fmap getCore $ asks getCurryState

-- ------------------------------------------------------------------------------

instance InitializerState CurryState where
    extensionId = const "Curry/CurryState"
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()

-- ------------------------------------------------------------------------------
-- | The Initializer for 'CurryState'. No arguments are required.

curryInitializer :: Initializer CurryState
curryInitializer = liftIO getCurryInitialState >>= mkInitializer . CurryState

-- ------------------------------------------------------------------------------

ixBase          :: FilePath
ixBase          = "./index"

-- ------------------------------------------------------------------------------

getCurryInitialState    :: IO Core
getCurryInitialState = do
  idx  <- loadIndex curryIndex
  infoM "Main" ("Curry index   loaded from file " ++ show curryIndex)
  infoM "Main" ("Curry index   contains " ++ show (sizeWords idx) ++ " words")
  doc  <- loadDocuments curryDocs
  infoM "Main" ("Curry docs    loaded from file " ++ show curryDocs )
  infoM "Main" ("Curry docs    contains " ++ show (sizeDocs doc) ++ " entries")
  return $ Core
             { index      = idx
             , documents  = doc
             }
    where
      curryIndex      = ixBase ++ "/ix.bin.idx"
      curryDocs       = ixBase ++ "/ix.bin.doc"
      infoM m msg   = hPutStrLn stderr $ m ++ ": " ++ msg

-- ------------------------------------------------------------------------------
