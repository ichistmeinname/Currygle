-- ----------------------------------------------------------------------------

{- |
  Module     : Main

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This is the entry point for this web server application.
-}

-- ----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Snap.Extension.Server
import System.Environment

import Application
import Site

main :: IO ()
main = do
       (arg:_) <- getArgs
       main2 arg
main2 :: FilePath -> IO ()
main2 arg = quickHttpServe (applicationInitializer arg) (site arg)


-- ----------------------------------------------------------------------------
