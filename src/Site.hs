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

module Site ( app ) where

import           Control.Applicative       ((<$>))
import           Control.Monad.IO.Class    (MonadIO(), liftIO)
import           Control.Monad.State.Lazy  (gets)

import           Data.ByteString           (ByteString)
import           Data.Function             (on)
import           Data.List                 (intercalate, isPrefixOf, sortBy)
import           Data.Map.Syntax           ((##))
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               (mconcat)
import qualified Data.Text          as T   (pack, unpack)
import qualified Data.Text.Encoding as T   (encodeUtf8, decodeUtf8)

import           Heist.Interpreted
import           Snap
import           Snap.Extras.JSON          (writeJSON)
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe       (serveDirectory)
import qualified Text.XmlHtml       as X

import Application
import CurryInfo
import CurrySearch ( InfoDoc (..), QRDocs (..), ResultDoc (..)
                   , InfoWord (..), QRWords (..)
                   , QueryFor, wordCompletions, queryResults )
import Helpers     ( showType, paren, constrTypeExpr, resultType )
import IndexTypes  ( loadCurryIndex)
import XmlHelper

-- ---------------------------------------------------------------------------
-- Snap specific part
-- ---------------------------------------------------------------------------

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "currygle" "Curry API search" Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  i <- liftIO $ loadCurryIndex True
  addRoutes $ [(contextPath, route routes)]
  return $ App h i
 where

contextPath :: ByteString
contextPath = "/kics2/currygle"

-- | Defines the routing of the web site.
-- It distinguishes between the front- and query-page
-- as well as the word completions.
-- All necessary files have to be stored in "resources/static".
routes :: [(ByteString, AppHandler ())]
routes =  [ ("/"           , frontpage   ) -- just render the frontpage
          , ("/completions", completions ) -- show word completions (JSON)
          , ("/results"    , results     ) -- show search results
          , ("/static"     , serveDirectory "static")
          ]

-- | Render the template file without substituting any tags.
frontpage :: AppHandler ()
frontpage = ifTop $ heistLocal (bindSplices splices) $ render "frontpage"
  where splices = mconcat
         [ "result"   ## return [example]
         , "oldquery" ## return [htmlTextNode ""]
         ]

-- | Return the list of word completions for the search text as JSON.
completions :: AppHandler ()
completions = do
  -- completns <- runQuery autocompletion
  completns <- runQuery wordCompletions
  putResponse $ setContentType "text/plain; charset=UTF-8"
              $ setResponseCode 200 emptyResponse
  writeJSON (extract completns)
 where
  extract = -- take _numDisplayedCompletions .
            map iwName . qwInfo

-- | Renders HTML page by substituing the tags <result />,
-- and < pager /> and the value $(oldQuery) for the template file.
results :: AppHandler ()
results = do
  docs  <- runQuery queryResults
  page  <- strToInt 1 <$> getRequestParam "page"
  query <- getRequestParam "query"
  let splices = mconcat [ "result"   ## resultSplice page docs
                        , "oldquery" ## return [htmlTextNode query]
                        , "pager"    ## pagerSplice query page docs
                        ]
  heistLocal (bindSplices splices) $ render "frontpage"

-- | Generates the HTML node of the search results.
resultSplice :: Int -> QRDocs -> Splice AppHandler
resultSplice _ (QRDocs _ [] [] [])
  = return ([htmlUl [noResults]])
resultSplice pageNum (QRDocs count mHits fHits tHits) =
  return (docCount count : itemsForPage)
 where
  itemsForPage = pageHits (map rExtra
                               (sortBy compare' itemsWithScore))
  compare' x y = case (compare `on` rScore) x y of
                     GT -> LT
                     EQ -> (compare `on` rName) x y
                     LT -> GT
  pageHits = take _hitsPerPage . drop ((pageNum - 1) * _hitsPerPage)
  itemsWithScore = map modDocsToListItem  mHits
                 ++ map funcDocsToListItem fHits
                 ++ map typeDocsToListItem tHits

debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn

-- | Generates the HTML node for the pagination, if necessary.
pagerSplice :: String -> Int -> QRDocs -> Splice AppHandler
pagerSplice query actPage (QRDocs count _ _ _)
  | count <= _hitsPerPage = return []
  | otherwise             = return (mkPagerLink query actPage pageCount)
  where pageCount = ceiling $ (toRational count) / (toRational _hitsPerPage)

-- Function to start the query processing
runQuery :: QueryFor a -> AppHandler a
runQuery queryfor = do
  qry <- getRequestParam "query"
  idx <- gets _index
  liftIO $ queryfor idx qry

-- Returns the value associated to a specific param from the Query-String
-- (i.e. query or page)
getRequestParam :: String -> AppHandler String
getRequestParam param = do
  query <- decodedParam $ T.encodeUtf8 $ T.pack param
  return $ T.unpack (T.decodeUtf8 query)
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
modDocsToListItem :: InfoDoc ModuleInfo -> ResultDoc X.Node
modDocsToListItem (InfoDoc title uri info _ score) =
  ResultDoc
   title
   score
   (resultNode title uri ("author", mAuthor info) (mDescription info) [])

-- Returns the HTML node for a result that is function.
-- The description and the name of the module are passed unmodified.
-- special title: NAME :: SIGNATURE or (OPERATOR) :: SIGNATURE
funcDocsToListItem :: InfoDoc FunctionInfo -> ResultDoc X.Node
funcDocsToListItem (InfoDoc title uri info _ score) =
  ResultDoc
   title
   score
   (resultNode sig uri (moduleText $ fModule info) (fDescription info) [])
 where
  sig         = paren (isInfixOp title) title ++ " :: " ++ ty
  ty          = showType (fModule info) False $ fSignature info
  isInfixOp s = head s `elem` ":!#$%&*+./<=>?@\\^|-~_"

-- Returns the HTML node for a result that is a type/data structure.
-- The description and module name are treated normally.
-- special title: data NAME = CONSTR1 | CONSTR2
typeDocsToListItem :: InfoDoc TypeInfo -> ResultDoc X.Node
typeDocsToListItem (InfoDoc _ uri info _ score) =
  ResultDoc
   title
   score
   (resultNode title uri (moduleText $ tModule info) (tDescription info) [])
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
