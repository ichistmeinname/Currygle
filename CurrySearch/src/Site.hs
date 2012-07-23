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


-- Return the HTML info text for the number of search results found (i.e. "Found 38 docs")
_docsMetaInfo :: QRDocs -> X.Node
_docsMetaInfo docs =
  htmlLiClass "info" [htmlTextNode $"Found " ++ (show $ qdDocCount docs) ++ " docs"]

-- Returns the HTML info text, if no resuls were found
_errorInfo :: X.Node
_errorInfo = htmlLiClass "info" [htmlTextNode "Sorry, there are no matching results."]

-- Number of hits shown per page
_hitsPerPage :: Int
_hitsPerPage = 10

-- Function to convert to list to a list of tuples (used for pairs of constructor names and signatures)
consPairs :: [String] -> [String] -> [(String, String)]
consPairs (e1:l1) (e2:l2) = (e1, e2) : consPairs l1 l2
consPairs _ _             = []    

-- Shortcut to build a tuples (used for the HTML node (css class, moduleName))
moduleText :: String -> (String, String)
moduleText text = ("module", text)

-- | Index data consisting of module, function and type information as triple.
coreIdx :: Application (CompactInverted, 
                        CompactInverted, 
                        CompactInverted)
coreIdx = do
  cCore <- curryCore
  return (modIndex cCore,fctIndex cCore, typeIndex cCore)

-- | Document data consisting of module, function and type information as triple.
coreDoc :: Application (SmallDocuments ModuleInfo, 
                        SmallDocuments FunctionInfo,
                        SmallDocuments TypeInfo)
coreDoc = do
  cCore <- curryCore
  return (modDocuments cCore, fctDocuments cCore, typeDocuments cCore)

-- Returns the HTML node for a result that is a module.
-- The title is the module name, the description is passed unchanged, but instead of the module name,
-- the author's name (the authors' names) are passed.
modDocsToListItem :: InfoDoc ModuleInfo -> X.Node
modDocsToListItem doc =
  makeResult (idTitle doc) (idUri doc) author (mDescription $ idInfo doc) []
 where author = ("author", mAuthor $ idInfo doc)

-- Returns the HTML node for a result that is function.
-- The description and the name of the module are passed unmodified.
-- special title: NAME :: SIGNATURE
funcDocsToListItem :: InfoDoc FunctionInfo -> X.Node
funcDocsToListItem doc =
  makeResult title (idUri doc) (moduleText $ fModule fInfo) (fDescription fInfo) []
 where title = idTitle doc ++ " :: " ++ signature
       signature = listToSignature $ typeToList $ fSignature fInfo
       fInfo = idInfo doc

-- Returns the HTML node for a result that is a type/data structure.
-- The description and module name are treated normally.
-- special title: data NAME = CONSTR1 | CONSTR2
typeDocsToListItem :: InfoDoc TypeInfo -> X.Node
typeDocsToListItem doc =
  makeResult title (idUri doc) (moduleText $ tModule tInfo) 
             (tDescription tInfo) (consPairs consName consSig)
 where consSig = map listToSignature $ map (fst . consToList (tName tInfo ++ 
                     (varIndex $ tVarIndex tInfo))) $ tSignature tInfo
       consName = map (snd . consToList ("")) $ tSignature tInfo  
       title = "data " ++ idTitle doc ++ constructors 
       constructors = ifNotEmpty consName $ " = " ++  L.intercalate " | " consName     
       tInfo = idInfo doc
            
-- | Generates the HTML node of the search results.
resultSplice :: Int -> QRDocs -> Splice Application
resultSplice pageNum docs = do
  let (mHits, fHits, tHits) = (qdModuleDocs docs,
                               qdFunctionDocs docs, 
                               qdTypeDocs docs)
      noHits = P.null mHits && P.null fHits && P.null tHits
      pageHits = L.take _hitsPerPage . L.drop ((pageNum-1)*_hitsPerPage)
      mItems = map modDocsToListItem  mHits
      fItems = map funcDocsToListItem fHits
      tItems = map typeDocsToListItem tHits 
      itemsForPage = pageHits (fItems ++ tItems ++ mItems) -- the order matters for the listings of the results
  if noHits
     then liftIO $ P.putStrLn "- keine Ergebnisse -" >> -- debug info
          (return $ [htmlUl [_errorInfo]])
     else (liftIO $ P.putStrLn $ "<" ++ (show $ P.length $ mHits) ++ ", "
                   ++ (show $ P.length $ fHits) ++ ", "
                   ++ (show $ P.length tHits) ++ ">") >> -- debug info
          (return $ ([_docsMetaInfo docs] ++ itemsForPage))

-- | Generates the HTML node for the pagination, if necessary.
pagerSplice :: String -> Int -> QRDocs -> Splice Application
pagerSplice query actPage docs = do
  let numberOfDocs = qdDocCount docs
      numberOfPages = ceiling $ (toRational numberOfDocs) / (toRational _hitsPerPage)
  if numberOfDocs <= 10 then return [] 
                        else return (mkPagerLink query actPage numberOfPages)

-- | Renders the template file without substituing any tags.
frontpage :: Application ()
frontpage = 
  ifTop $ heistLocal (bindSplices []) $ render "frontpage"

-- | Generates the HTML node for the searchfield after a processed query.
oldQuerySplice :: Splice Application
oldQuerySplice = do
  query <- lift $ decodedParam "query"
  let query' = T.unpack (E.decodeUtf8 query)
  return $ [htmlTextNode query']
 where decodedParam p = fromMaybe "" <$> getParam p

-- Returns the value associated to a specific param from the Query-String (i.e. query or page)
getQueryStringParam :: String -> Application String
getQueryStringParam param = do
  let decodedParam p = fromMaybe "" <$> getParam p
  query <- decodedParam $ encodeUtf8 $ T.pack param
  return $ T.unpack (E.decodeUtf8 query)

-- Function to start the query processing
queryFunction :: Application (Query -> IO MFTResult)
queryFunction = do
  (docM, docF, docT) <- coreDoc
  (idxM, idxF, idxT) <- coreIdx
  return $ queryResult idxM docM idxF docF idxT docT

-- | Renders HTML page by substituing the tags <result />, and < pager /> and the value $(oldQuery)
--   for the template file.
processquery :: Application ()
processquery = do
  strPage    <- getQueryStringParam "page"
  query      <- getQueryStringParam "query"
  queryFunc' <- queryFunction
  docs       <- liftIO $ queryResultDocs queryFunc' query
  let splices = [("result", resultSplice (strToInt 1 strPage) docs),
                 ("oldquery", oldQuerySplice),
                 ("pager", pagerSplice query (strToInt 1 strPage) docs)]
  heistLocal (bindSplices splices) $ render "frontpage"

-- Converts a list to a JSON-Array
toJSONArray :: Int -> [InfoWord] -> String
toJSONArray n srwh
    = encodeStrict $
      showJSONs (map (\ (InfoWord w1 _) -> w1 {- ++ " (" ++ (show h1) ++ ")" -} ) (L.take n srwh))

-- Number of word completions that are sent to the javascript
numDisplayedCompletions :: Int
numDisplayedCompletions = 20

-- | Returns the list of found word completions for the typed text to the javascript.
completions :: Application ()
completions = do
  query <- getQueryStringParam "query"
  queryFunc' <- queryFunction
  queryResultWords' <- liftIO $ wordCompletions queryFunc' query
  putResponse myResponse
  writeText (T.pack $ toJSONArray numDisplayedCompletions $ qwInfo queryResultWords')
  where
  myResponse = setContentType "text/plain; charset=utf-8" . setResponseCode 200 $ emptyResponse

-- | Defines the routing of the web site. It differs between the front- and querypage
--   as well as the word completions. All necessary files have to be stored in "resources/static".
site :: Application ()
site = route
       [ ("/",          frontpage)     -- just render the frontpage
       , ("/querypage", processquery)  -- show search results
       , ("/completions", completions) -- show word completions (javascript)
       ]
       <|> serveDirectory "resources/static"

