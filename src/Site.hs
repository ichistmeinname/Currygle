{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Site
Description :  Routes, handlers and web representation
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

This module defines all used routes and handlers.
Furthermore, the representation of the web site is handled
and gathered by the function site, which is exported.
-}

module Site ( site ) where

import           Control.Applicative       ((<|>), (<$>))
import           Control.Monad.Reader      (MonadIO, asks, liftIO)
import           Data.List                 (intercalate, isPrefixOf)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text          as T   (pack, unpack)
import qualified Data.Text.Encoding as E   (encodeUtf8, decodeUtf8)

import           Snap.Extension.Heist.Impl (render, heistLocal)
import           Snap.Util.FileServe       (serveDirectory)
import           Snap.Types
import           Text.JSON                 (encodeStrict, showJSONs)
import           Text.Templating.Heist     (Splice, bindSplices)
import qualified Text.XmlHtml       as X

import Application ( Application, HasCurryIndex (..) )
import CurryInfo
import CurrySearch ( InfoDoc (..), QRDocs (..), InfoWord (..), QRWords (..)
                   , QueryFor, wordCompletions, queryResults )
import Helpers     ( showType, paren, constrTypeExpr, resultType )
import XmlHelper

-- ---------------------------------------------------------------------------
-- Snap specific part
-- ---------------------------------------------------------------------------

-- | Defines the routing of the web site.
-- It distinguishes between the front- and query-page
-- as well as the word completions.
-- All necessary files have to be stored in "resources/static".
site :: Application ()
site = dir "kics2" $ dir "currygle" $ route
  [ ("/"           , frontpage   ) -- just render the frontpage
  , ("/completions", completions ) -- show word completions (JSON)
  , ("/results"    , results     ) -- show search results
  ] <|> serveDirectory "resources/static"

-- | Render the template file without substituting any tags.
frontpage :: Application ()
frontpage
  = ifTop $ heistLocal (bindSplices [("result", return [example])])
  $ render "frontpage"

-- | Return the list of word completions for the search text as JSON.
completions :: Application ()
completions = do
  completns <- runQuery wordCompletions
  putResponse $ setContentType "text/plain; charset=UTF-8"
              $ setResponseCode 200 emptyResponse
  writeText $ T.pack $ toJSON $ extract completns
  where
  extract = take _numDisplayedCompletions . map iwName . qwInfo
  toJSON  = encodeStrict . showJSONs

-- | Renders HTML page by substituing the tags <result />,
-- and < pager /> and the value $(oldQuery) for the template file.
results :: Application ()
results = do
  docs  <- runQuery queryResults
  page  <- strToInt 1 <$> getRequestParam "page"
  query <- getRequestParam "query"
  let splices = [ ("result"  , resultSplice         page docs)
                , ("oldquery", return [htmlTextNode query]   )
                , ("pager"   , pagerSplice    query page docs)
                ]
  heistLocal (bindSplices splices) $ render "frontpage"

-- | Generates the HTML node of the search results.
resultSplice :: Int -> QRDocs -> Splice Application
resultSplice _ (QRDocs _ [] [] [])
  = debug "- keine Ergebnisse -" >> return ([htmlUl [noResults]])
resultSplice pageNum (QRDocs count mHits fHits tHits) = do
  debug $ "<" ++ (show $ length $ mHits) ++ ", " ++ (show $ length $ fHits)
            ++ ", " ++ (show $ length tHits) ++ ">"
  return $ docCount count : itemsForPage
  where
  itemsForPage = pageHits (fItems ++ tItems ++ mItems)
  pageHits = take _hitsPerPage . drop ((pageNum - 1) * _hitsPerPage)
  mItems = map modDocsToListItem  mHits
  fItems = map funcDocsToListItem fHits
  tItems = map typeDocsToListItem tHits
  -- the order matters for the listings of the results

debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn

-- | Generates the HTML node for the pagination, if necessary.
pagerSplice :: String -> Int -> QRDocs -> Splice Application
pagerSplice query actPage (QRDocs count _ _ _)
  | count <= _hitsPerPage = return []
  | otherwise             = return (mkPagerLink query actPage pageCount)
  where pageCount = ceiling $ (toRational count) / (toRational _hitsPerPage)

-- Function to start the query processing
runQuery :: QueryFor a -> Application a
runQuery queryfor = do
  query <- getRequestParam "query"
  state <- asks getCurryIndex
  liftIO $ queryfor state query

-- Returns the value associated to a specific param from the Query-String
-- (i.e. query or page)
getRequestParam :: String -> Application String
getRequestParam param = do
  query <- decodedParam $ E.encodeUtf8 $ T.pack param
  return $ T.unpack (E.decodeUtf8 query)
  where decodedParam p = fromMaybe "" <$> getParam p

-- ---------------------------------------------------------------------------
-- XML creation
-- ---------------------------------------------------------------------------

-- Number of hits shown per page
_hitsPerPage :: Int
_hitsPerPage = 10

-- Number of word completions that are sent via JSON
_numDisplayedCompletions :: Int
_numDisplayedCompletions = 20

-- Shortcut to build a tuple (used for the HTML node (css class, moduleName))
moduleText :: String -> (String, String)
moduleText text = ("module", text)

-- Returns the HTML node for a result that is a module.
-- The title is the module name, the description is passed unchanged,
-- but instead of the module name,
-- the author's name (the authors' names) are passed.
modDocsToListItem :: InfoDoc ModuleInfo -> X.Node
modDocsToListItem (InfoDoc title uri info _ _)
  = makeResult title uri ("author", mAuthor info) (mDescription info) []

-- Returns the HTML node for a result that is function.
-- The description and the name of the module are passed unmodified.
-- special title: NAME :: SIGNATURE or (OPERATOR) :: SIGNATURE
funcDocsToListItem :: InfoDoc FunctionInfo -> X.Node
funcDocsToListItem (InfoDoc title uri info _ _) =
  makeResult sig uri (moduleText $ fModule info) (fDescription info) []
  where
  sig         = paren (isInfixOp title) title ++ " :: " ++ ty
  ty          = showType (fModule info) False $ fSignature info
  isInfixOp s = head s `elem` ":!#$%&*+./<=>?@\\^|-~_"

-- Returns the HTML node for a result that is a type/data structure.
-- The description and module name are treated normally.
-- special title: data NAME = CONSTR1 | CONSTR2
typeDocsToListItem :: InfoDoc TypeInfo -> X.Node
typeDocsToListItem (InfoDoc _ uri info _ _)
  = makeResult title uri (moduleText $ tModule info) (tDescription info) []
  where
  title | tIsTypeSyn info = typeSynTitle
        | otherwise       = dataTitle
  typeSynTitle = "type " ++ tName info ++ " = "
                 ++ showType "" False (snd (head consNames))
  consNames    = constrTypeExpr info
  dataTitle    = "data " ++ showType "" False (resultType info)
                 ++ if null consNames then "" else
                       " = " ++  intercalate " | " (showConstrs info)

showConstrs :: TypeInfo -> [String]
showConstrs tInfo = map showConstr (tSignature tInfo)
  where
  showConstr (("Prelude",":"), [tExpr1,tExpr2])
    = showType "" False tExpr1 ++ " : " ++ showType "" False tExpr2
  showConstr ((modName, typeName), tExprList)
    | modName == "Prelude" && "(," `isPrefixOf` typeName
    = "(" ++ intercalate ", " (map (showType "" True) tExprList) ++ ")"
    | otherwise
    = typeName ++ " " ++ intercalate " " (map (showType "" True) tExprList)
