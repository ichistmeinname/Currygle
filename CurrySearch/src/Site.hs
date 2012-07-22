{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Site
Description :  Routes, handlers and web representation
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

This module defines all used routes and handlers. Furthermore the representation of the web site is handled and gathered by the function site, which is exported.
-}

module Site ( site ) where

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
import Data.Maybe
import Data.Text                as T hiding (map)
import Data.Text.Encoding       as E

import Holumbus.Query.Language.Grammar

import Prelude                  as P

import Snap.Extension.Heist.Impl
import Snap.Types
import Snap.Util.FileServe

import Text.JSON hiding (Result)
import Text.Templating.Heist
import qualified Text.XmlHtml   as X


-- ------------------------------------------------------------------------------
-- 
--  some constants
-- 
-- ------------------------------------------------------------------------------

-- | number of hits shown per page
hitsPerPage :: Int
hitsPerPage = 10

-- ------------------------------------------------------------------------------
-- | number of word completions send in response to the Ajax request

numDisplayedCompletions :: Int
numDisplayedCompletions = 20

-- ------------------------------------------------------------------------------
-- | Index data consisting of module, function and type information as triple

coreIdx :: Application (CompactInverted, 
                        CompactInverted, 
                        CompactInverted)
coreIdx = do
  cCore <- curryCore
  return (CoreData.modIndex cCore, 
          CoreData.fctIndex cCore, 
          CoreData.typeIndex cCore)

-- ------------------------------------------------------------------------------
-- | Document data consisting of module, function and type information as triple

coreDoc :: Application (SmallDocuments ModuleInfo, 
                        SmallDocuments FunctionInfo,
                        SmallDocuments TypeInfo)
coreDoc = do
  cCore <- curryCore
  return (CoreData.modDocuments cCore, 
          CoreData.fctDocuments cCore, 
          CoreData.typeDocuments cCore)

-- ------------------------------------------------------------------------------
-- | Function to start a query 

queryFunction :: Application (Query -> IO MFTResult)
queryFunction = do
  (docM, docF, docT) <- coreDoc
  (idxM, idxF, idxT) <- coreIdx
  return $ queryResult idxM docM idxF docF idxT docT

-- ------------------------------------------------------------------------------
-- | get the value associated to a specific param from the Query-String

getQueryStringParam :: String -> Application String
getQueryStringParam param = do
  let decodedParam p = fromMaybe "" <$> getParam p
  query <- decodedParam $ encodeUtf8 $ T.pack param
  return $ T.unpack (E.decodeUtf8 query)

modDocsToListItem :: InfoDoc ModuleInfo -> X.Node
modDocsToListItem doc =
  makeResult (idTitle doc) (idUri doc) author (mDescription $ idInfo doc) []
 where author = ("author", mAuthor $ idInfo doc)

funcDocsToListItem :: InfoDoc FunctionInfo -> X.Node
funcDocsToListItem doc =
  makeResult title (idUri doc) (moduleText $ fModule fInfo) (fDescription fInfo) []
 where title = idTitle doc ++ " :: " ++ signature
       signature = listToSignature $ typeToList $ fSignature fInfo
       fInfo = idInfo doc

typeDocsToListItem :: InfoDoc TypeInfo -> X.Node
typeDocsToListItem doc =
  makeResult title (idUri doc) (moduleText $ tModule tInfo) (tDescription tInfo) (consPairs consName consSig)
 where consSig = map listToSignature $ map (fst . consToList (tName tInfo ++ (varIndex $ tVarIndex tInfo))) 
                                     $ tSignature tInfo
       consName = map (snd . consToList ("")) $ tSignature tInfo  
       title = "data " ++ idTitle doc ++ constructors 
       constructors = ifNotEmpty consName $ " = " ++  L.intercalate " | " consName     
       tInfo = idInfo doc
            
-- ------------------------------------------------------------------------------
-- | creates the HTML info text describing the search result (i.e. "Found 38 docs")

docsMetaInfo :: QRDocs -> X.Node
docsMetaInfo docs =
  htmlLiClass "info" [htmlTextNode $"Found " ++ (show $ qdDocCount docs) ++ " docs"]

errorInfo :: X.Node
errorInfo = htmlLiClass "info" [htmlTextNode "Sorry, there are no matching results."]

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
      heistLocal (bindSplices [("result", return [])]) $
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
    strPage    <- getQueryStringParam "page"
    query      <- getQueryStringParam "query"
    queryFunc' <- queryFunction
    docs       <- liftIO $ queryResultDocs queryFunc' query
    let indexSplices = [("result", resultSplice (strToInt 1 strPage) docs),
                        ("oldquery", oldQuerySplice),
                        ("pager", pagerSplice query (strToInt 1 strPage) docs)]
    heistLocal (bindSplices indexSplices) $ render "frontpage"

-- | generates the HTML node to be inserted into "<result />"

resultSplice :: Int -> QRDocs -> Splice Application
resultSplice pageNum docs = do
  let (mHits, fHits, tHits) = (qdModuleDocs docs,
                               qdFunctionDocs docs, 
                               qdTypeDocs docs)
      noHits = P.null mHits && P.null fHits && P.null tHits
      pageHits = L.take hitsPerPage . L.drop ((pageNum-1)*hitsPerPage)
      mItems = map modDocsToListItem  mHits
      fItems = map funcDocsToListItem fHits
      tItems = map typeDocsToListItem tHits 
      -- test = (\(InfoDoc idTitle _ _ _ _) -> map (fst . (consToList idTitle))) tHits 
      itemsForPage = pageHits (fItems ++ tItems ++ mItems)
  if noHits
     then liftIO $ P.putStrLn "- keine Ergebnisse -" >> -- debug info
          (return $ [htmlUl [errorInfo]])
     else (liftIO $ P.putStrLn $ "<" ++ (show $ P.length $ mHits) ++ ", "
                   ++ (show $ P.length $ fHits) ++ ", "
                   ++ (show $ P.length tHits) ++ ">") >> -- debug info
          (return $ ([docsMetaInfo docs] ++ itemsForPage))

-- | generates the HTML node to be inserted into "<oldquery />"

oldQuerySplice :: Splice Application
oldQuerySplice = do
  let decodedParam p = fromMaybe "" <$> getParam p
  query <- lift $ decodedParam "query"
  let query' = T.unpack (E.decodeUtf8 query)
  return $ [htmlTextNode query']

-- | generates the HTML node to be inserted into "<pager />"

pagerSplice :: String -> Int -> QRDocs -> Splice Application
pagerSplice query actPage docs = do
  let numberOfDocs = qdDocCount docs
      numberOfPages = ceiling $ (toRational numberOfDocs) / (toRational hitsPerPage)
  if numberOfDocs <= 10 then return [] 
                        else return (mkPagerLink query actPage numberOfPages)

-- ------------------------------------------------------------------------------
-- 
--  completions
-- 
-- ------------------------------------------------------------------------------
-- returns the list of found completions to the Ajax-caller

completions :: Application ()
completions = do
  query <- getQueryStringParam "query"
  queryFunc' <- queryFunction
  queryResultWords' <- liftIO $ wordCompletions queryFunc' query
  putResponse myResponse
  writeText (T.pack $ toJSONArray numDisplayedCompletions $ qwInfo queryResultWords')
  where
  myResponse = setContentType "text/plain; charset=utf-8" . setResponseCode 200 $ emptyResponse

-- convert List to JSON-Array
toJSONArray :: Int -> [InfoWord] -> String
toJSONArray n srwh
    = encodeStrict $
      showJSONs (map (\ (InfoWord w1 _) -> w1 {- ++ " (" ++ (show h1) ++ ")" -} ) (L.take n srwh))

