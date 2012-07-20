{- |
Module      :  Main
Description :  There's nothing to see here but a simple function to start the server.
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