{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Currygle where

import           Control.Lens.TH
import           Data.IORef
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Snap
import           Snap.Snaplet.Heist

data App = App
  { _heist :: Snaplet (Heist App)
  , _curry :: CurryState
  }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

description :: String
description = "Currygle API search"

appInit :: SnapletInit App App
appInit = makeSnaplet "currygle" description Nothing $ do
  hs <- nestSnaplet "" heist $ heistInit "templates"
  cs <- liftIO $ loadCurryState
--   addRoutes [ ("/hello", writeText "hello world")
--             , ("/fooname", with foo namePage)
--             , ("/barname", with bar namePage)
--             , ("/company", companyHandler)
--             ]
--   wrapSite (<|> heistServe)
  return $ App hs cs
