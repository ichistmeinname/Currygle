{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Application
Description :  Defines the application's monad and related informations
Copyright   :  (c) Sandra Dylus, Björn Peemöller
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

Defines the application's state.
-}
module Application where

import           Snap               (Snaplet, Handler, makeLenses, subSnaplet)
import           Snap.Snaplet.Heist (HasHeist (..), Heist)

import           IndexTypes         (CurryIndex)

data App = App
  { _heist :: Snaplet (Heist App)
  , _index :: CurryIndex
  }

type AppHandler = Handler App App

makeLenses [''App]

instance HasHeist App where
    heistLens = subSnaplet heist
