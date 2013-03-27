{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Main
Description :  Main function to start the server.
Copyright   :  (c) Sandra Dylus, Björn Peemöller
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

Entry point to start server.
-}

module Main (main) where

import           Control.Exception         (SomeException, try)
import qualified Data.Text            as T (unpack)
import           System.Posix.Process      (getProcessID)
import           System.IO                 (hPutStrLn, stderr)

import           Snap
import           Snap.Loader.Prod          (loadSnapTH)

import           FilesAndLoading           (pidFile)
import           Site                      (app)

main :: IO ()
main = do
  savePID
  (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                        'getActions
                                        ["resources/templates"])
  _ <- try $ httpServe conf $ site :: IO (Either SomeException ())
  cleanup

-- |Save the PID of the current process.
savePID :: IO ()
savePID = do
  pid <- getProcessID
  putStrLn ("Running with PID " ++ show pid)
  writeFile pidFile (show pid)

-- | This action loads the config used by this application.
getConf :: IO (Config Snap ())
getConf = commandLineConfig defaultConfig

-- | This function generates the the site handler and cleanup action
-- from the configuration.
getActions :: Config Snap () -> IO (Snap (), IO ())
getActions _ = do
    (msgs, site, cleanup) <- runSnaplet app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
