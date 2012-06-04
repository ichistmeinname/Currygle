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
    { core :: Core
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
    curryCore = fmap core $ asks getCurryState

-- ------------------------------------------------------------------------------

instance (MonadSnap m, HasCurryState s) => MonadCurry (ReaderT s m) where
    curryCore = fmap core $ asks getCurryState

-- ------------------------------------------------------------------------------

instance InitializerState CurryState where
    extensionId = const "Curry/CurryState"
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()

-- ------------------------------------------------------------------------------
-- | The Initializer for 'CurryState'. No arguments are required.

curryInitializer :: Initializer CurryState
curryInitializer = liftIO curryInitState >>= mkInitializer . CurryState

-- ------------------------------------------------------------------------------

ixBase :: FilePath
ixBase = "./index"

-- ------------------------------------------------------------------------------

curryInitState :: IO Core
curryInitState = do
  idxMod  <- loadIndex curryModIndex
  infoMsg "index" (sizeWords idxMod) "words"
  docMod  <- loadModDocuments curryModDocs
  infoMsg "documents" (sizeDocs docMod) "entries"
  idxFct  <- loadIndex curryFctIndex
  infoMsg "index" (sizeWords idxFct) "words"
  docFct  <- loadFctDocuments curryFctDocs
  infoMsg "documents" (sizeDocs docFct) "entries"
  idxType <- loadIndex curryTypeIndex
  infoMsg "index" (sizeWords idxType) "words"
  docType <- loadTypeDocuments curryTypeDocs
  infoMsg "documents" (sizeDocs docType) "entries"
  -- hPutStrLn stderr "doc: "
  -- hPutStrLn stderr $ show (toMap docMod)
  -- hPutStrLn stderr "idx: "
  -- hPutStrLn stderr $ show (toList idxMod)
  return $ Core
             { modIndex      = idxMod
             , modDocuments  = docMod
             , fctIndex      = idxFct
             , fctDocuments  = docFct
             , typeIndex     = idxType
             , typeDocuments = docType
             }
    where
      curryModIndex  = ixBase ++ "/ix-mod.bin.idx"
      curryModDocs   = ixBase ++ "/ix-mod.bin.doc"
      curryFctIndex  = ixBase ++ "/ix-fct.bin.idx"
      curryFctDocs   = ixBase ++ "/ix-fct.bin.doc"
      curryTypeIndex = ixBase ++ "/ix-type.bin.idx"
      curryTypeDocs  = ixBase ++ "/ix-type.bin.doc"
      infoMsg str1 fIdxOrDoc str2 = 
          hPutStrLn stderr $ "Init process: Curry " ++ str1 ++ " was loaded successfully and contains " ++ show (fIdxOrDoc) ++ " " ++ str2

-- ------------------------------------------------------------------------------
