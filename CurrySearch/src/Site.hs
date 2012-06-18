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
import Helpers hiding (htmlLink)
import IndexTypes
import CurrySearch
import CurryState
import CurryInfo

import Control.Applicative
import Control.Monad.Trans

import Data.List                as L
import Data.Maybe
import Data.Text                as T
import Data.Text.Encoding       as E
import Data.Char                as C (toUpper)      

import Holumbus.Query.Language.Grammar
import Holumbus.Query.Result

import Prelude                  as P

import Snap.Extension.Heist
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


-- ------------------------------------------------------------------------------
-- | number of words contained in the teaser text

numTeaserWords :: Int
numTeaserWords = 30

-- ------------------------------------------------------------------------------
-- | number of word completions send in response to the Ajax request

numDisplayedCompletions :: Int
numDisplayedCompletions = 20

-- ------------------------------------------------------------------------------
-- | Index data consisting of module, function and type information as tripel

coreIdx :: Application (CompactInverted, 
                        CompactInverted, 
                        CompactInverted)
coreIdx = do
  cCore <- curryCore
  return $ (CoreData.modIndex cCore, 
            CoreData.fctIndex cCore, 
            CoreData.typeIndex cCore)

-- ------------------------------------------------------------------------------
-- | Document data consisting of module, function and type information as tripel

coreDoc :: Application (SmallDocuments ModuleInfo, 
                        SmallDocuments FunctionInfo,
                        SmallDocuments TypeInfo)
coreDoc = do
  cCore <- curryCore
  return $ (CoreData.modDocuments cCore, 
            CoreData.fctDocuments cCore, 
            CoreData.typeDocuments cCore)

-- ------------------------------------------------------------------------------
-- | Function to start a query 

queryFunction :: Application (Query -> IO (Result ModuleInfo, 
                                           Result FunctionInfo,
                                           Result TypeInfo))
queryFunction = do
  (docM, docF, docT) <- coreDoc
  (idxM, idxF, idxT) <- coreIdx
  return $ localQuery idxM docM idxF docF idxT docT

-- ------------------------------------------------------------------------------
-- | get the value associated to a specific param from the Query-String

getQueryStringParam :: String -> Application String
getQueryStringParam param = do
  let decodedParam p = fromMaybe "" <$> getParam p
  query <- decodedParam $ encodeUtf8 $ T.pack param
  return $ T.unpack (E.decodeUtf8 query)

-- ------------------------------------------------------------------------------
-- | Creates a sequence of html-tags to display a search result. It is placed as html-list with a link to the document, the document title, the description as a teaser and optional texts.
--  i.e.
--  <li>
--    <ul>
--      <li><a href="linkToDocumentFound">titleOfDocumentFound</a></li>
--      <li>optionalItems</li>
--      <li>teaser</li>
--    </ul>
--  </li>

docHitToListItem :: [(String, a -> String)] -> (a -> String) -> SRDocHit a -> X.Node
docHitToListItem is teaser docHit
    = htmlListItem "searchResult" $ 
      htmlList "" $ htmlLink htmlUri "searchResultTitle" (srTitle docHit)
                  ++ optionalList
                  ++ htmlLink htmlUri "descriptionTeaser" teaserText
  where
      teaserText
          = (++ "...") . L.unwords . L.take numTeaserWords . L.words . teaser $ srInfo docHit
      optionalList = P.concat $ 
                     P.map (\(itemTitle,item) -> htmlLink htmlUri 
                                                           itemTitle 
                                                           (item $ srInfo docHit))
                            is
      htmlUri = srUri docHit

htmlLink :: (String) -> String -> String -> [X.Node]
htmlLink htmlUri tagName text = [htmlLink' "" htmlUri $
                                 htmlListItem tagName $
                                 htmlTextNode $ text]
                      


makeTitle :: String -> String
makeTitle [] = []
makeTitle (t:ext) = C.toUpper t : ext ++ ": "
            
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
-- 
--  site
-- 
-- ------------------------------------------------------------------------------
-- | defines routes through our site

site :: FilePath -> Application ()
site args = route
       [ ("/",          frontpage)
       , ("/querypage", processquery)  -- render HTML Page with found Document-Hits
       , ("/completions", completions) -- return List of Completions to Ajax-caller
       ]
       -- <|> serveDirectory "resources/static"
       <|> serveDirectory (args++"resources/static")

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
  let (mHits, fHits, tHits) = (srModuleDocHits searchResultDocs,
                               srFunctionDocHits searchResultDocs, 
                               srTypeDocHits searchResultDocs)
      noHits = P.null mHits && P.null fHits && P.null tHits
      pageHits h = L.take hitsPerPage $ L.drop ((pageNum-1)*hitsPerPage) h
      mItems = P.map (docHitToListItem [("author", mAuthor)] mDescription)  
                     (pageHits $ mHits)
      fItems = P.map (docHitToListItem [("module", fModule),
                     ("signature", (\a -> L.intercalate "->" $ typeSignature (fModule a) (snd $ fSignature a)))]
                     fDescription)
                     (pageHits $ fHits)
      tItems = P.map (docHitToListItem [("module", tModule), 
                   ("signature", (\a -> P.concat $ P.concatMap (consToList) (tSignature a)))]
                   tDescription) 
                   (pageHits $ tHits) 
      -- tItems = P.map (docHitToListItem (modSigList (tModule) (\a -> P.concat $ tSignature a)))
      --                                   tDescription) 
      --                (pageHits $ tHits)
      -- modSigList fMod fSig = [("module", fMod), ("signature", 
      --                        (\a -> L.intercalate "->" (typeSignature fMod (snd $ fSig a))))]

  -- debug informations
  if noHits
    then liftIO $ P.putStrLn "- keine Ergebnisse -"
    else do
      liftIO $ P.putStrLn $ "<" ++ (show $ P.length $ mHits) ++ ", "
                                ++ (show $ P.length $ fHits) ++ ", "
                                ++ (show $ P.length tHits) ++ ">"
  if noHits
     then return $ [htmlList "" [errorInfo]]
     else return $ [htmlList "" ([docHitsMetaInfo searchResultDocs] 
                                ++ mItems 
                                ++ fItems 
                                ++ tItems)]

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
  let resultCount =  (L.length $ srModuleDocHits searchResultDocs) + 
                     (L.length $ srFunctionDocHits searchResultDocs) + 
                     (L.length $ srTypeDocHits searchResultDocs)
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
  queryFunc' <- queryFunction
  searchResultWords' <- liftIO $ getWordCompletions query' $ queryFunc'
  putResponse myResponse
  writeText (T.pack $ toJSONArray numDisplayedCompletions $ srWordHits searchResultWords')
  where
  myResponse = setContentType "text/plain; charset=utf-8" . setResponseCode 200 $ emptyResponse

-- convert List to JSON-Array
toJSONArray :: Int -> [SRWordHit] -> String
toJSONArray n srwh
    = encodeStrict $
      showJSONs (P.map (\ (SRWordHit w1 _) -> w1 {- ++ " (" ++ (show h1) ++ ")" -} ) (L.take n srwh))

-- ------------------------------------------------------------------------------

