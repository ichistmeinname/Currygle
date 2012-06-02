{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

{-

This is where all the routes and handlers are defined for our site. The
'site' function combines everything together and is exported by this module.

-}

-- ----------------------------------------------------------------------------

module Site ( site )

where

import Application
import CoreData
import Helpers
import IndexTypes
import CurrySearch
import CurryState
import CurryInfo

import Control.Applicative
import Control.Monad.Trans

import Data.List                as L
import Data.Map                 as M
import Data.Maybe
import Data.Text                as T
import Data.Text.Encoding       as E

import Holumbus.Index.Common
import Holumbus.Query.Language.Grammar
import Holumbus.Query.Result

import Prelude                  as P

import Snap.Extension.Heist
import Snap.Types
import Snap.Util.FileServe

import Text.JSON
import Text.Templating.Heist
import qualified Text.XmlHtml   as X


-- ------------------------------------------------------------------------------
-- 
--  some constants
-- 
-- ------------------------------------------------------------------------------


-- ------------------------------------------------------------------------------
-- | number of words contained in the teaser text

numTeaserWords :: Int
numTeaserWords = 30

-- ------------------------------------------------------------------------------
-- | number of word completions send in response to the Ajax request

numDisplayedCompletions :: Int
numDisplayedCompletions = 20

-- ------------------------------------------------------------------------------
-- | get the Index-Data

getCoreIdx :: Application CompactInverted
getCoreIdx = do
  core <- curryCore
  return $ CoreData.fctIndex core

-- ------------------------------------------------------------------------------
-- | get the Document-Data

getCoreDoc :: Application (SmallDocuments FunctionInfo)
getCoreDoc = do
  core <- curryCore
  return $ CoreData.fctDocuments core

-- ------------------------------------------------------------------------------
-- | the function that does the query

queryFunction :: Application (Query -> IO (Holumbus.Query.Result.Result FunctionInfo))
queryFunction = do
  doc <- getCoreDoc
  idx <- getCoreIdx
  return $ localQuery idx doc

-- ------------------------------------------------------------------------------
-- | get the value associated to a specific param from the Query-String

getQueryStringParam :: String -> Application String
getQueryStringParam param = do
  let decodedParam p = fromMaybe "" <$> getParam p
  query <- decodedParam $ encodeUtf8 $ T.pack param
  return $ T.unpack (E.decodeUtf8 query)

-- ------------------------------------------------------------------------------
-- | creates a HTML List-Item containing a List with the link to the document found, the teasertext and the ranking-score
--  i.e.
--  <li>
--    <ul>
--      <li><a href="linkToDocumentFound">titleOfDocumentFound</a></li>
--      <li>teaserText</li>
--      <li>rankingScoreOfDocumetFound</li>
--    </ul>
--  </li>

docHitToListItem :: SRDocHit -> X.Node
docHitToListItem docHit
    = htmlListItem "searchResult" $ subList
    where
      -- auth = mAuthor . srModuleInfo $ docHit
      -- authText
      --     = if (auth == "")
      --       then ""
      --       else "erstellt von " ++ auth
     
      subList
          = htmlList "" subListItems
      subListItems
          = [ htmlLink' "" (srUri docHit) $
              htmlListItem "searchResultTitle" $
              htmlTextNode . srTitle $
              docHit
            ]
            -- ++
            -- [ htmlLink' "" (srUri docHit) $
            --   htmlListItem "searchResultModified" $
            --   htmlTextNode $
            --   authText
            -- ]
            ++ [ htmlLink' "" (srUri docHit) mkContentContext ]
      mkContentContext
          =  htmlListItem "teaserText" $ htmlTextNode teaserText
      teaserText
          = (++ "...") . L.unwords . L.take numTeaserWords . L.words . fDescription $ srFunctionInfo docHit
     

-- ------------------------------------------------------------------------------
-- | creates the HTML info text describing the search result (i.e. "Found 38 docs in 0.0 sec.")

docHitsMetaInfo :: SearchResultDocs -> X.Node
docHitsMetaInfo searchResultDocs
    = htmlListItem "info" $
      htmlTextNode $
        "Found " ++
        (show $ srDocCount searchResultDocs) ++
        " docs in " ++
        (show $ srTime searchResultDocs) ++ " sec."

errorInfo :: X.Node
errorInfo = htmlListItem "info" $
            htmlTextNode $
            "Sorry, there are no matching results."


-- ------------------------------------------------------------------------------
-- | maybe transform the search-query into a normalized date string.
--  Result: (tranformedStringOrOriginalString, whetherOrNotTheStringIsADate)

-- maybeNormalizeQuery :: String -> (String, Bool)
-- maybeNormalizeQuery query
--     =  (either id id normalizedDateOrQuery, isDate)
--     where
--       normalizedDates
--           = D.dateRep2NormalizedDates . D.extractDateRep $ query
--       isDate
--           = not $ L.null normalizedDates
--       normalizedDateOrQuery
--           = if not isDate
--             then Left query
--             else Right $ L.head normalizedDates

-- ------------------------------------------------------------------------------
-- 
--  site
-- 
-- ------------------------------------------------------------------------------
-- | defines routes through our site

site :: Application ()
site = route
       [ ("/",          frontpage)
       , ("/querypage", processquery)  -- render HTML Page with found Document-Hits
       , ("/completions", completions) -- return List of Completions to Ajax-caller
       ]
       <|> serveDirectory "resources/static"

-- ------------------------------------------------------------------------------
-- 
--  frontpage
-- 
-- ------------------------------------------------------------------------------
-- | renders the front page.
--  simply display "frontpage.tpl" Template-file without substituting any <... /> Tags

frontpage :: Application ()
frontpage
    = ifTop $
      heistLocal (bindSplices [("result", return [examples])]) $
      render "frontpage"

-- ------------------------------------------------------------------------------
--
--  processquery
-- 
-- ------------------------------------------------------------------------------

-- | render HTML Page with the doc-hits found.
--  display "frontpage.tpl" Template-file with substituting following Tags:
--    <result />
--    <oldquery />
--    <pager />

processquery :: Application ()
processquery = do
  query   <- getQueryStringParam "query"
  -- dateRep <- liftIO $ extractDateRepM query
  -- (transformedQuery, numOfTransforms) <-liftIO $ dateRep2stringWithTransformedDates dateRep
  -- let hasDate = (numOfTransforms > 0)
  -- liftIO $ P.putStrLn $ "<" ++ transformedQuery ++ ">" -- print debug info to console
  queryFunc' <- queryFunction
  searchResultDocs <- liftIO $ getIndexSearchResults query queryFunc'
  strPage <- getQueryStringParam "page"
  let intPage = strToInt 1 strPage
  let indexSplices = [ ("result", resultSplice intPage searchResultDocs)
                     , ("oldquery", oldQuerySplice)
                     , ("pager", pagerSplice query intPage searchResultDocs)
                     ]
  heistLocal (bindSplices indexSplices) $ render "frontpage"

-- | generates the HTML node to be inserted into "<result />"

resultSplice :: Int -> SearchResultDocs -> Splice Application
resultSplice pageNum searchResultDocs = do
  let _docHits = srDocHits searchResultDocs
  let items = P.map (docHitToListItem) (L.take hitsPerPage $ L.drop ((pageNum-1)*hitsPerPage) $ _docHits)
  -- debug informations
  if P.null $ _docHits 
    then liftIO $ P.putStrLn "- keine Ergebnisse -"
    else do
      liftIO $ P.putStrLn $ "<" ++ (show $ P.length $ _docHits) ++ ">"
  let infos = [docHitsMetaInfo searchResultDocs]
  if P.null $ _docHits
     then return $ [htmlList "" [errorInfo]]
     else return $ [htmlList "" (infos ++ items)]

-- | generates the HTML node to be inserted into "<oldquery />"

oldQuerySplice :: Splice Application
oldQuerySplice = do
  let decodedParam p = fromMaybe "" <$> getParam p
  query <- lift $ decodedParam "query"
  let query' = T.unpack (E.decodeUtf8 query)
  return $ [htmlTextNode query']

-- | generates the HTML node to be inserted into "<pager />"

pagerSplice :: String -> Int -> SearchResultDocs -> Splice Application
pagerSplice query actPage searchResultDocs = do
  let resultCount =  L.length $ srDocHits searchResultDocs
  let numberOfPages = min maxPages (ceiling $ (toRational resultCount) / (toRational hitsPerPage))
  return $ L.map (mkPagerLink query actPage) [1..numberOfPages]

-- ------------------------------------------------------------------------------
-- 
--  completions
-- 
-- ------------------------------------------------------------------------------
-- returns the list of found completions to the Ajax-caller

completions :: Application ()
completions = do
  query' <- getQueryStringParam "query"
  -- let (query', isDate) = maybeNormalizeQuery query'' -- determine if its a date
  query <- return query'
  queryFunc' <- queryFunction
  searchResultWords' <- liftIO $ getWordCompletions query $ queryFunc'
  let searchResultWords = srWordHits searchResultWords'
  putResponse myResponse
  writeText (T.pack $ toJSONArray numDisplayedCompletions $ searchResultWords)
  where
  myResponse = setContentType "text/plain; charset=utf-8" . setResponseCode 200 $ emptyResponse

-- convert List to JSON-Array
toJSONArray :: Int -> [SRWordHit] -> String
toJSONArray n srwh
    = encodeStrict $
      showJSONs (P.map (\ (SRWordHit w1 _) -> w1 {- ++ " (" ++ (show h1) ++ ")" -} ) (L.take n srwh))

-- ------------------------------------------------------------------------------

