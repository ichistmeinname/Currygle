{- |
Module      :  CurrySearch
Description :  The Heart of the search engine.
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

This module holds the functions to interpret a given query.
It produces a result that consists of possible
word completions and documents, that match the query.
-}
module CurrySearch where

import           Data.Binary
import           Data.Function
import qualified Data.List              as L
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)

import           Holumbus.Index.Common
import           Holumbus.Index.Common.DocIdMap  (sizeDocIdMap, emptyDocIdMap)
import           Holumbus.Query.Fuzzy            (FuzzyConfig (..), englishReplacements)
import           Holumbus.Query.Language.Grammar (Query (..))
import           Holumbus.Query.Processor        (ProcessConfig (..), processQuery)
import           Holumbus.Query.Ranking
import           Holumbus.Query.Result

import CurryInfo
import IndexTypes  (CurryIndex (..))
import QueryParser (parse)

-- | Shortcut for the result triple.
type MFTResult = (Result ModuleInfo, Result FunctionInfo, Result TypeInfo)

type QueryFor a = CurryIndex -> String -> IO a

-- | Returns only the documents of a query result.
queryResults :: QueryFor QRDocs
queryResults state = either noResults makeQuery . parse
  where
  noResults _     = return emptyQRDocs
  makeQuery query = do
    results      <- queryResult state query
    (rM, rF, rT) <- defaultRanks results
    docHitsToResult (docHits rM, docHits rF, docHits rT)

-- | Returns only the word completions of a query result.
wordCompletions :: QueryFor QRWords
wordCompletions state str = either (const (return emptyQRWords)) makeQuery (parse str)
  where
  -- queryWithoutSpecifier = prepareWordCompletionQuery str
  makeQuery query = do
    let queryWithSpecifier = wordCompletionSpecifier query
    results <- queryResult state queryWithSpecifier
    (rrM, rrF, rrT) <- defaultRanks results
    return (sortWords (foldr M.union (wordHits rrM) [wordHits rrF, wordHits rrT]))

-- | Processes a query for a given module, function and type indexer pair.
queryResult :: CurryIndex -> Query -> IO MFTResult
queryResult (CurryIndex ixM docM ixF docF ixT docT) q
  = return (process ixM docM, process ixF docF, process ixT docT)
  where process ix doc = processQuery processCfg ix doc q

-- The default weights for the contexts.
_defaultRankTable :: RankTable
_defaultRankTable =
  [ ("function"   , 1.0 )
  , ("type"       , 0.75)
  , ("signature"  , 0.5 )
  , ("module"     , 0.5 )
  , ("author"     , 0.2 )
  , ("description", 0.1 ) ]

-- The context weights for a word completion, only function, type, and module names are important.
_wordCompletionRankTable :: RankTable
_wordCompletionRankTable =
  [ ("function", 1.0)
  , ("type", 0.5)
  , ("module", 0.5) ]

-- For this search engine, the documents and word completions have different rank tables.
_defaultRankCfg :: RankConfig a
_defaultRankCfg = RankConfig (docRankWeightedByCount _defaultRankTable)
                             (wordRankWeightedByCount _wordCompletionRankTable)

-- Converts matching documents (DocHits) to a InfoDoc structure that holds information
-- about the document and the corresponding curryInfo.
infoDoc :: (Binary a) => a -> (DocId, (DocInfo a, DocContextHits)) -> InfoDoc a
infoDoc emptyInfo (_, (DocInfo (Document title' uri' info') score', contextMap'))
    = InfoDoc title' uri' (fromMaybe emptyInfo info') contextMap' score'

-- Sorts the documents by score considering the (possibly) applied rank table
-- and returns a list of these documents as info document data structure.
sortInfoDoc :: (Binary a) => a -> DocHits a -> [InfoDoc a]
sortInfoDoc emptyInfo info = map (infoDoc emptyInfo) $ docData info
  where docData = L.sortBy pred' . toListDocIdMap
        pred' info1 info2 =
          case (compare `on` (docScore . fst . snd)) info1 info2 of
               EQ -> (compare `on` (title . document . fst . snd)) info1 info2
               LT -> GT
               GT -> LT

-- | Converts a triple of curryInfos to a QRDocs data structure.
--   It sorts the documents by a given rank and counts the total number of matching documents.
docHitsToResult :: (DocHits ModuleInfo,
                    DocHits FunctionInfo,
                    DocHits TypeInfo) -> IO QRDocs
docHitsToResult (m, f, t) = return $ QRDocs size
                            (sortInfoDoc emptyModuleInfo m)
                            (sortInfoDoc emptyFunctionInfo f)
                            (sortInfoDoc emptyTypeInfo t)
 where size = sizeDocIdMap m + sizeDocIdMap f + sizeDocIdMap t

-- Sorts word/doc completions by score that considers an (possibly) applied rank table
-- and returns a word/doc completion data structure.
sortWords :: WordHits -> QRWords
sortWords h =
  QRWords (M.size h) wordData
 where wordData = L.reverse $ L.sortBy (compare `on` iwName)
                  (map (\ (word, (wordInfo, _)) -> InfoWord word (wordScore wordInfo))
                       $ M.toList h)

-- When processing word completions consider only the function, module and type contexts.
wordCompletionSpecifier :: Query -> Query
wordCompletionSpecifier (Specifier cs query) =
  Specifier ("function":"module":"type":"author":cs) query
wordCompletionSpecifier query                =
  Specifier ["function","module","type","author"] query

-- | The function to apply a rank table (weights) to a given result.
defaultRanks :: MFTResult -> IO MFTResult
defaultRanks (m, f, t) = return (rank _defaultRankCfg m,
                                 rank _defaultRankCfg f,
                                 rank _defaultRankCfg t)

-- | A RankTable represents a context and its given score to allow weighted query results.
type RankTable  = [(Context, Score)]

-- | Defaul configuration to process query. Search fuzzy (with switched adjacent characters and
--   some specified replacements for the english language), with an optimized query and no limit to
--   found words or documents.
processCfg :: ProcessConfig
processCfg =
  ProcessConfig (FuzzyConfig True True 1.0 englishReplacements) True 0 0

-- | Representation of word completions.
-- It contains the number of possible completions and a list of these words.
data QRWords = QRWords { qwCount :: Int, qwInfo :: [InfoWord] }
  deriving Show

emptyQRWords :: QRWords
emptyQRWords = QRWords 0 []

-- | A possible word completion holds a name (the word itself) and a score.
data InfoWord = InfoWord { iwName :: String, iwScore :: Score }
  deriving Show

-- |The documents that match a query are divided into three groups,
-- corresponding to the curryInfo data.
-- So the data holds these three lists and an attribute that represents
-- the total number of documents.
data QRDocs = QRDocs
  { qdDocCount     :: Int
  , qdModuleDocs   :: [InfoDoc ModuleInfo]
  , qdFunctionDocs :: [InfoDoc FunctionInfo]
  , qdTypeDocs     :: [InfoDoc TypeInfo]
  } deriving Show

-- | Empty constructor.
emptyQRDocs :: QRDocs
emptyQRDocs = QRDocs 0 [] [] []

qrDocsToQrWords :: QRDocs -> QRWords
qrDocsToQrWords (QRDocs count mds fcts tps) =
  QRWords count (map infoDocToInfoWord mds
                ++ map infoDocToInfoWord fcts
                ++ map infoDocToInfoWord tps)

-- |A document stores information about its title, uri and score.
-- Furthermore, it consits of a mapping of the contexts
-- (i.e. function, module, type, author etc)
-- and its words and the corresponding curryInfo data
-- (i.e. FunctionInfo, ModuleInfo, TypeInfo).
data InfoDoc a = InfoDoc
  { idTitle      :: String
  , idUri        :: String
  , idInfo       :: a
  , idContextMap :: M.Map Context DocWordHits
  , idScore      :: Score
  } deriving Show

infoDocToInfoWord :: InfoDoc a -> InfoWord
infoDocToInfoWord (InfoDoc name _ _ _ score) = InfoWord name score

data ResultDoc a = ResultDoc { rName :: String, rScore :: Score, rExtra :: a }
