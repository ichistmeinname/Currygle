{- |
Module      :  CurrySearch
Description :  The Heart of the search engine.
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

This module holds the functions to interpret a given query.
-}

module CurrySearch where

import           Data.Function
import qualified Data.List              as L
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)

import           Holumbus.Index.Common

import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Processor
import           Holumbus.Query.Result
import           Holumbus.Query.Ranking
import           Holumbus.Query.Fuzzy

import           System.CPUTime 

import           Parser
import           IndexTypes
import           CurryInfo
import           FilesAndLoading

-- | number of hits shown per page
hitsPerPage :: Int
hitsPerPage = 10

-- ------------------------------------------------------------

-- | max number of pages
maxPages :: Int
maxPages = 10

-- ------------------------------------------------------------

-- Representation of all Document-Hits and Word-Completions found

data SearchResult
    = SearchResult
      { srDocs  :: SearchResultDocs
      , srWords :: SearchResultWords
      }

data SearchResultDocs
    = SearchResultDocs
      { srTime      :: Float
      , srDocCount  :: Int
      , srModuleDocHits   :: [SRDocHit ModuleInfo]
      , srFunctionDocHits :: [SRDocHit FunctionInfo]
      , srTypeDocHits :: [SRDocHit TypeInfo]
      } deriving Show

data SearchResultWords
    = SearchResultWords
      { srWordCount :: Int
      , srWordHits  :: [SRWordHit]
      }

data  SRDocHit a
    = SRDocHit
      { srTitle         :: String
      , srScore         :: Float
      , srInfo          :: a
      , srUri           :: String
      , srContextMap    :: M.Map Context DocWordHits 
      } deriving Show

data SRWordHit
    = SRWordHit
      { srWord :: String
      , srHits :: Int
      }

-- Ranking for different kinds of Contexts

type RankTable  = [(Context, Score)]

defaultRankTable :: RankTable
defaultRankTable
    = [ ("Name", 1.0)
      , ("Signature",  0.5)
      , ("Module",      0.2)
      , ("Author",    1.0)
      , ("Description", 2.0)
      , ("Other",  0.2)
      ]

defaultRankCfg :: RankConfig a
defaultRankCfg
    = RankConfig
      (docRankWeightedByCount  defaultRankTable)
      (wordRankWeightedByCount defaultRankTable)

-- | Create the configuration for the query processor.

processCfg :: ProcessConfig
processCfg
    = ProcessConfig (FuzzyConfig True True 1.0 germanReplacements) True 100 500

-- | Perform a query on a local index.

localQuery :: CompactInverted -> SmallDocuments ModuleInfo 
           -> CompactInverted -> SmallDocuments FunctionInfo 
           -> CompactInverted -> SmallDocuments TypeInfo 
           -> Query -> IO (Result ModuleInfo, 
                           Result FunctionInfo,
                           Result TypeInfo)
localQuery ixM docM ixF docF ixT docT q
    = return (queryL ixM docM, queryL ixF docF, queryL ixT docT)
  where queryL ix doc = processQuery processCfg ix doc q


-- | get all Search Results

getAllSearchResults :: String -> (Query -> IO (Result ModuleInfo, 
                                               Result FunctionInfo, 
                                               Result TypeInfo)) 
                              -> IO SearchResult
getAllSearchResults q f
    = do docs'  <- getIndexSearchResults q f
         words' <- getWordCompletions    q f
         return $ SearchResult docs' words'

-- | Insert the time needed for request into SearchResultDocs data type.
--
--  Delete all elements in the list of search-results such that the list is uniq
--  by the title of a found document.
--  Shorten the search-result list to (hitsPerPage * maxPages) elements.
--  Adapt the displayed number of docs found 

mkDocSearchResult :: Float -> SearchResultDocs -> SearchResultDocs
mkDocSearchResult requestTime searchResultDocs
    = SearchResultDocs requestTime dislayedNumOfHits (docHits' srModuleDocHits) (docHits' srFunctionDocHits) (docHits' srTypeDocHits)
    where
      docHits' info = take (hitsPerPage * maxPages) $
                      uniqByTitle $
                      info searchResultDocs
    -- not a good idea: (length $ uniqByTitle $ srDocHits searchResultDocs), since the whole list would be processed by the O(n^2) algorithm "uniqByTitle"

      dislayedNumOfHits
          = if numElemsShortList == hitsPerPage * maxPages
            then numElemsLongList
            else numElemsShortList

      numElemsShortList = (length $ docHits' srModuleDocHits) 
                          + (length $ docHits' srFunctionDocHits)
                          + (length $ docHits' srTypeDocHits)

      -- this is the length without filtering!
      numElemsLongList = srDocCount searchResultDocs

-- | helper for mkDocSearchResult:
--  delete all elements in the list of search-results such that the list is uniq
--  by the title of a found document.
--  This is an O(n^2) algorithm but we truncate the result list of uniqByTitle to (hitsPerPage*maxPages) elements,
--  so only these elements are computed due to lazy evaluation.
--  This has proven to be the best method to get rid of many equal search results.

uniqByTitle :: [SRDocHit a] -> [SRDocHit a]
uniqByTitle []     = []
uniqByTitle (x:xs) = x : uniqByTitle (deleteByTitle (srTitle x) xs)
  where
    deleteByTitle t = filter (\ listItem -> srTitle listItem /= t)


-- | get only Document Search Results (without Word-Completions)

getIndexSearchResults :: String -> (Query -> IO (Result ModuleInfo,
                                                 Result FunctionInfo,
                                                 Result TypeInfo)) 
                                -> IO SearchResultDocs
getIndexSearchResults q f
    = either printError makeQuery $ prepareQuery q
    where
      printError _
          = return $ SearchResultDocs 0.0 0 [] [] [] 
      makeQuery pq
          = do t1 <- getCPUTime
               (rM, rF, rT)  <- f pq -- This is where the magic happens!
               (rrM, rrF, rrT) <- return (rank defaultRankCfg rM, 
                                          rank defaultRankCfg rF,
                                          rank defaultRankCfg rT)
               docsSearchResult <- getDocHits (docHits rrM, docHits rrF, docHits rrT)
               t2 <- getCPUTime
               let d = (fromIntegral (t2 - t1) / 1000000000000.0) :: Float
               return $ mkDocSearchResult d docsSearchResult

-- | get only Word-Completions (without Document Search Results)

getWordCompletions :: String -> (Query -> IO (Result ModuleInfo,
                                              Result FunctionInfo,
                                              Result TypeInfo)) 
                             -> IO SearchResultWords
getWordCompletions q f
    = either printError makeQuery $ prepareQuery q
    where
      printError _
          = return $ SearchResultWords 0 []
      makeQuery pq
          = do (rM, rF, rT) <- f pq -- This is where the magic happens!
               (rrM, rrF, rrT) <- return (rank defaultRankCfg rM,
                                          rank defaultRankCfg rF,
                                          rank defaultRankCfg rT)
               -- getWordHits (wordHits rrM, wordHits rrF, wordHits rrT))
               getWordHits (wordHits rrF)
-- | convert Document-Hits to SearchResult

getDocHits :: (DocHits ModuleInfo, 
               DocHits FunctionInfo, 
               DocHits TypeInfo) -> IO SearchResultDocs
getDocHits (m, f, t) = return $ SearchResultDocs 0.0 size (map docModuleInfoToSRDocHit $ docData m)
                                                                  (map docFunctionInfoToSRDocHit $ docData f)
                                                                  (map docTypeInfoToSRDocHit $ docData t)
    where
      docData d = L.reverse $ 
                  L.sortBy (compare `on` (docScore . fst . snd)) $
                  toListDocIdMap d
      size      = sizeDocIdMap m + sizeDocIdMap f + sizeDocIdMap t

docModuleInfoToSRDocHit :: (DocId, (DocInfo ModuleInfo, DocContextHits)) -> SRDocHit ModuleInfo
docModuleInfoToSRDocHit (_, (DocInfo (Document title' uri' info') score', contextMap'))
    = SRDocHit title' score' (fromMaybe emptyModuleInfo info') uri' contextMap'

docFunctionInfoToSRDocHit :: (DocId, (DocInfo FunctionInfo, DocContextHits)) -> SRDocHit FunctionInfo
docFunctionInfoToSRDocHit (_, (DocInfo (Document title' uri' info') score', contextMap'))
    = SRDocHit title' score' (fromMaybe emptyFunctionInfo info') uri' contextMap'

docTypeInfoToSRDocHit :: (DocId, (DocInfo TypeInfo, DocContextHits)) -> SRDocHit TypeInfo
docTypeInfoToSRDocHit  (_, (DocInfo (Document title' uri' info') score', contextMap'))
    = SRDocHit title' score' (fromMaybe emptyTypeInfo info') uri' contextMap'

-- | convert Word-Completions to SearchResult

getWordHits :: WordHits -> IO SearchResultWords
getWordHits h
    = return $ SearchResultWords (M.size h) (getWordHits' wordData)
    where
      wordData
          = L.reverse $
              L.sortBy (compare `on` snd)
                   (map (\ (c, (_, o)) ->
                             (c, M.fold (\m r -> r + sizeDocIdMap m) 0 o)
                        ) (M.toList h)
                   )
      getWordHits' []
          = []
      getWordHits' ((c, s) : xs)
          = SRWordHit c s : getWordHits' xs

-- ----------------------------------------------------------------------------
