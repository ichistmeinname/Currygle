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

import System.Posix.Process (getProcessID)

import Snap.Extension.Server (quickHttpServe)

import Application     (appInitializer)
import FilesAndLoading (pidFile)
import Site            (site)

main :: IO ()
main = do
  pid <- getProcessID
  putStrLn ("Running with PID " ++ show pid)
  writeFile pidFile (show pid)
  quickHttpServe appInitializer site
