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

import           Data.Binary
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

import           Parser
import           IndexTypes
import           CurryInfo
import           Helpers

-- | number of hits shown per page
hitsPerPage :: Int
hitsPerPage = 10

-- ------------------------------------------------------------

-- -- | max number of pages
-- maxPages :: Int
-- maxPages = 10

-- ------------------------------------------------------------

-- Representation of all Document-Hits and Word-Completions found

data SearchResult
    = SearchResult
      { srDocs  :: SearchResultDocs
      , srWords :: SearchResultWords
      }

data SearchResultDocs
    = SearchResultDocs
      { srDocCount  :: Int
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

data SRWordHit = SRWordHit { srWord :: String, srHits :: Int}

-- Ranking for different kinds of Contexts

type RankTable  = [(Context, Score)]

defaultRankTable :: RankTable
defaultRankTable = 
    [("Function", 1.0),
     ("Type", 0.75),
     ("TheModule", 0.25),
     ("Signature", 0.75),
     ("Module", 0.5),
     ("Author", 0.2),
     ("Description", 0.5),
     ("Other", 0.2)]

wordCompletionRankTable :: RankTable
wordCompletionRankTable = [("Function", 1.0), ("Type", 0.5), ("Module", 0.5)]

defaultRankCfg :: RankConfig a
defaultRankCfg = RankConfig (docRankWeightedByCount defaultRankTable)
                            (wordRankWeightedByCount wordCompletionRankTable)

type MFTResult = (Result ModuleInfo, Result FunctionInfo, Result TypeInfo)

defaultRanks :: MFTResult -> IO MFTResult 
defaultRanks (m, f, t) = return (rank defaultRankCfg m, 
                                 rank defaultRankCfg f,
                                 rank defaultRankCfg t)

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

-- -- | get all Search Results

-- allSearchResults :: String -> (Query -> IO (Result ModuleInfo, 
--                                             Result FunctionInfo, 
--                                             Result TypeInfo)) 
--                               -> IO SearchResult
-- allSearchResults q f
--     = do docs'  <- getIndexSearchResults q f
--          words' <- getWordCompletions    q f
--          return $ SearchResult docs' words'

-- -- | Insert the time needed for request into SearchResultDocs data type.
-- --
-- --  Delete all elements in the list of search-results such that the list is uniq
-- --  by the title of a found document.
-- --  Shorten the search-result list to (hitsPerPage * maxPages) elements.
-- --  Adapt the displayed number of docs found 

-- addTime :: Float -> SearchResultDocs -> SearchResultDocs
-- addTime requestTime searchResultDocs
--     = SearchResultDocs requestTime dislayedNumOfHits (docHits' srModuleDocHits) (docHits' srFunctionDocHits) (docHits' srTypeDocHits)
--     where
--       docHits' info = take (hitsPerPage * maxPages) $
--                       uniqByTitle $
--                       info searchResultDocs
--     -- not a good idea: (length $ uniqByTitle $ srDocHits searchResultDocs), since the whole list would be processed by the O(n^2) algorithm "uniqByTitle"

--       dislayedNumOfHits
--           = if numElemsShortList == hitsPerPage * maxPages
--             then numElemsLongList
--             else numElemsShortList

--       numElemsShortList = (length $ docHits' srModuleDocHits) 
--                           + (length $ docHits' srFunctionDocHits)
--                           + (length $ docHits' srTypeDocHits)

--       -- this is the length without filtering!
--       numElemsLongList = srDocCount searchResultDocs

-- -- | helper for addTime:
-- --  delete all elements in the list of search-results such that the list is uniq
-- --  by the title of a found document.
-- --  This is an O(n^2) algorithm but we truncate the result list of uniqByTitle to (hitsPerPage*maxPages) elements,
-- --  so only these elements are computed due to lazy evaluation.
-- --  This has proven to be the best method to get rid of many equal search results.

-- uniqByTitle :: [SRDocHit a] -> [SRDocHit a]
-- uniqByTitle []     = []
-- uniqByTitle (x:xs) = x : uniqByTitle (deleteByTitle (srTitle x) xs)
--   where
--     deleteByTitle t = filter (\ listItem -> srTitle listItem /= t)


-- | get only Document Search Results (without Word-Completions)

searchResultDocs :: String -> (Query -> IO MFTResult) -> IO SearchResultDocs
searchResultDocs q f
    = either noResults makeQuery $ prepareQuery q
    where
      noResults _
          = return $ SearchResultDocs 0 [] [] [] 
      makeQuery pq
          = do results      <- f pq
               (rM, rF, rT) <- defaultRanks results
               resultDocs   <- docHitsToResult (docHits rM, docHits rF, docHits rT)
               return resultDocs

-- | get only Word-Completions (without Document Search Results)
wordCompletions :: String -> (Query -> IO MFTResult) -> IO SearchResultWords
wordCompletions q f = 
    --either noResults 
    makeQuery $ prepare q
  where noResults _ = return $ SearchResultWords 0 []
        prepare = wordCompletionSpecifier . prepareWordCompletionQuery
        makeQuery pq = 
          do results <- f pq -- This is where the magic happens!
             (rrM, rrF, rrT) <- defaultRanks results
             
             wordResults (foldr M.union (wordHits rrM) [wordHits rrF, wordHits rrT])

wordCompletionSpecifier :: String -> Query
wordCompletionSpecifier = Specifier ["Function","TheModule","Type"] . Word

prepareWordCompletionQuery :: String -> String
prepareWordCompletionQuery queryString = 
  concat $ filter (not . (":" `L.isPrefixOf`)) $ splitOnWhitespace queryString

-- | convert Document-Hits to SearchResult

docHitsToResult :: (DocHits ModuleInfo, 
                    DocHits FunctionInfo, 
                    DocHits TypeInfo) -> IO SearchResultDocs
docHitsToResult (m, f, t) = return $ SearchResultDocs size 
                            (sortedSRDocHits emptyModuleInfo m)
                            (sortedSRDocHits emptyFunctionInfo f)
                            (sortedSRDocHits emptyTypeInfo t)
 where size = sizeDocIdMap m + sizeDocIdMap f + sizeDocIdMap t

sortedSRDocHits :: (Binary a) => a -> DocHits a -> [SRDocHit a]
sortedSRDocHits emptyInfo info = map (docInfoToSRDocHit emptyInfo) $ docData info
  where docData = L.sortBy (compare `on` docHitScore) . toListDocIdMap
        docHitScore = docScore . fst . snd

-- | convert Word-Completions to SearchResult

wordResults :: WordHits -> IO SearchResultWords
wordResults h
    = return $ SearchResultWords (M.size h) (wordHits' wordData)
    where
      wordData
          =   L.reverse $
              L.sortBy (compare `on` snd)
                   (map (\ (c, (_, o)) ->
                             (c, M.fold (\m r -> r + sizeDocIdMap m) 0 o)
                        ) (M.toList h))
      wordHits' [] = []
      wordHits' ((c, s) : xs) = SRWordHit c s : wordHits' xs

docInfoToSRDocHit :: (Binary a) => a -> (DocId, (DocInfo a, DocContextHits)) -> SRDocHit a
docInfoToSRDocHit emptyInfo (_, (DocInfo (Document title' uri' info') score', contextMap'))
    = SRDocHit title' score' (fromMaybe emptyInfo info') uri' contextMap'
