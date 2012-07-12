{- |
Module      :  Main
Description :  
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

Entry point to start server.
-}

module Main where

import Snap.Extension.Server

import Application
import Site

main :: IO ()
main = quickHttpServe appInitializer site


-- ----------------------------------------------------------------------------
