{- |
Module      :  CurryIndexer
Description :  Creates an index for the given data
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

This module creates the index and documents
for one (or more) given cdoc file(s).
Three pairs of index and documents are stored:
for the module, function and type information.
-}
module Main (main) where

import Control.Monad       (liftM)
import Data.Binary         (Binary , Word32)
import Data.List.Split     (splitOn)

import System.Directory   (renameFile, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.FilePath    ((</>), addTrailingPathSeparator, takeBaseName)
import System.IO          (hPutStrLn, stderr)

import CurryInfo
import FilesAndLoading
import Helpers            ( biasedWord, splitOnWhitespace, constrTypeExpr
                          , signatureComponents )
import IndexTypes

import Holumbus.Crawler.IndexerCore (IndexerState (..))
import Holumbus.Index.CompactSmallDocuments (docTable2smallDocTable, idToSmallDoc)
import Holumbus.Index.Common.Occurences
import Holumbus.Index.Common.DocId (DocId, addDocId)
import Holumbus.Index.Common.DocIdMap (maxKeyDocIdMap)
import Holumbus.Index.Common (HolIndex (..), HolDocuments (..), HolDocIndex (..), Document (..))

main :: IO ()
main = getArgs >>= processArgs

-- Analyzes arguments of the command line
processArgs :: [String] -> IO ()
processArgs args = case args of
  [fPath,             "--n"] -> readFilePaths True  fPath
  [fPath,             "--u"] -> readFilePaths False fPath
  [cdocPath, uriPath, "--n"] -> startIndexer True  cdocPath uriPath
  [cdocPath, uriPath, "--u"] -> startIndexer False cdocPath uriPath
  _                          -> badUsage

badUsage :: IO ()
badUsage = hPutStrLn stderr _howToUseMessage >> exitFailure

_howToUseMessage :: String
_howToUseMessage = unlines
  [ "No no no, you don't use it the right way. Let me give you a hint."
  , "Try it like this:"
  , "  curryIndexer <cdoc_directory> <documentation_uri> [--n|--u]"
  , "Or maybe like this:"
  , "  curryIndexer <specification_file> [--n|--u]"
  , ""
  , "<specification_file> is expected to contain multiple lines consisting"
  , "  of pairs of a CDOC directory and the corresponding URI, seperated"
  , "  by a semicolon."
  , ""
  , "'--u' updates your current index with the new data."
  , "'--n' creates a new index with the given data."
  ]

-- Reads txt-file with pairs of cdoc and uri paths that are seperated by ';'
-- to create or update (new flag) the index
readFilePaths :: Bool -> FilePath -> IO ()
readFilePaths new fPath = do
  file <- readFile fPath
  prepareFilePaths new $ filePathList file
  where filePathList = concatMap (splitOn ";") . splitOn "\n"

-- Recursive function to start indexing,
-- either a new index is created or the existing index is updated (new flag)
prepareFilePaths :: Bool -> [FilePath] -> IO ()
prepareFilePaths new (cdocPath:uriPath:xs) =
  startIndexer new cdocPath uriPath >> prepareFilePaths False xs
prepareFilePaths _ [_] = return ()
prepareFilePaths _ _   = putStr _howToUseMessage

-- Main function to start the index creation
startIndexer :: Bool -> FilePath -> FilePath -> IO ()
startIndexer new cdocP uriP = do
  putStr "Writing index ..."
  files <- filter (hasExtension _currydocExtension) `liftM`
           getDirectoryContents cdocP
  writeIndex new uriPath $ map (cdocP </>) files
  putStrLn "done."
 where uriPath  = addTrailingPathSeparator uriP

-- | Writes a new index when flag is true,
-- merges new index with existing one otherwise
writeIndex :: Bool -> FilePath -> [FilePath] -> IO ()
writeIndex _   _       []    = putStr "NO cdoc files found."
writeIndex new uriPath files = do
  (nM,nF,nT) <- foldr (occurenceCheck new uriPath) emptyStates files
  if new then writeIndex' moduleNames (nM,nF,nT)
         else mergeIdxDoc moduleNames (loadCurryIndex False) (nM, nF, nT)
 where emptyStates = return emptyCurryIndexerStates
       moduleNames = map takeBaseName files


-- | Triple of the index-documents-pairs
type CurryIndexerStates =
  ( -- | Pair of index and documents of the type ModuleInfo
    HolumbusState ModuleInfo
  , -- | Pair of index and documents of the type FunctionInfo
    HolumbusState FunctionInfo
  , -- | Pair of index and documents of the type TypeInfo
    HolumbusState TypeInfo
  )

emptyCurryIndexerStates :: CurryIndexerStates
emptyCurryIndexerStates = (emptyState, emptyState, emptyState)
  where emptyState = emptyIndexerState emptyInverted emptyDocuments

makeIndexerState :: Inverted -> Documents a -> HolumbusState a
makeIndexerState = IndexerState


-- Writes index and documents with a function that converts the types automatically
-- (Inverted to CompactInverted, Documents a to SmallDocuments a)
writeIndex' :: [String] -> CurryIndexerStates -> IO ()
writeIndex' moduleList (curryModState, curryFctState, curryTypeState) = do
  putStr "." >> writeSearchBin _moduleIndexPath   curryModState
  putStr "." >> writeSearchBin _functionIndexPath curryFctState
  putStr "." >> writeSearchBin _typeIndexPath     curryTypeState
  writeFile (listExtension _moduleListPath) (show moduleList)

-- Singleton shortcut for Occurences
occ :: DocId -> Word32 -> Occurrences
occ = singletonOccurrence

-- Adds an occurences to a tuple, returns a triple
addOcc :: Occurrences -> (a,b) -> (a,b,Occurrences)
addOcc occurrence (a,b) = (a,b,occurrence)

-- Adds description context to a string
description :: String -> [(String, String)]
description = map (addContext "description")
            . filter (not . biasedWord) . splitOnWhitespace

-- Adds signature context to a signature string and all its suffixes
signature :: [String] -> [(String, String)]
signature = map (addContext "signature")

-- Adds function contexts to a list of given function names
functionContext :: [String] -> [(String, String)]
functionContext = map (addContext "function") . filter (not . null)

-- Adds author context to a string
author :: String -> [(String, String)]
author = map (addContext "author") . splitOnWhitespace

-- Returns tupel of two strings
addContext :: String -> String -> (String, String)
addContext context s = (context, s)

-- Is a given module in the moduleList?
writeOrPass :: String -> IO CurryIndexerStates -> IO CurryIndexerStates
writeOrPass moduleName ioAction = do
  mList <- loadModuleList
  if moduleName `elem` mList then return emptyCurryIndexerStates
                             else ioAction

-- Checks, if the module exists in the index
occurenceCheck :: Bool -> FilePath -> FilePath -> IO CurryIndexerStates
               -> IO CurryIndexerStates
occurenceCheck new uriPath cdocPath states
  | new       = index
  | otherwise = writeOrPass (takeBaseName cdocPath) index
 where index = prepareIndex uriPath cdocPath states

-- Helper function for writing index
prepareIndex :: FilePath -> FilePath -> IO CurryIndexerStates
             -> IO CurryIndexerStates
prepareIndex uriPath cdocPath states = do
  (cMod, cFct, cTyp) <- states
  curryDoc <- readFromFile cdocPath
  putStr "."
  return $ indexAndDocuments curryDoc uriPath (cMod, cFct, cTyp)

-- | Function to build a document.
doc :: Binary a => String -> (a -> String) -> URI a -> a -> Document a
doc uriPath fiName uriType info = Document
  { title  = fiName info
  , uri    = uriPath ++ localP
  , custom = Just info
  }
  where
  localP = case uriType of
    ModuleURI    baseUri      -> baseUri info ++ ".html"
    FctOrTypeURI baseUri name -> baseUri info ++ ".html" ++ "#" ++ name info

-- | Data to represent uri,
-- functions and types use anchors, i.e. moduleName.html#functionName.
data URI a
  = ModuleURI    (a -> String)
  | FctOrTypeURI (a -> String) (a -> String)

-- | Function to build index.
idx :: (a -> DocId -> [(String, String, Occurrences)]) -> a -> DocId -> Inverted
idx contextList info i = fromList emptyInverted $ contextList info i

-- |  Main indexer method to build indexes and documents.
ixDoc :: Binary a => (a -> DocId -> [(String, String, Occurrences)]) ->
                       [a] -> [Document a] -> HolumbusState a -> HolumbusState a
ixDoc contextList (info:infos) (doc1:docs) (IndexerState ix dc) =
  let (docId, docs') = insertDoc dc doc1
      idx'           = mergeIndexes ix $ idx contextList info docId
  in ixDoc contextList infos docs (makeIndexerState idx' docs')
ixDoc _ _ _ is = is

-- Calls ixDoc to create index and documents for the module, functions and types
indexAndDocuments :: CurryInfo -> FilePath  -> CurryIndexerStates -> CurryIndexerStates
indexAndDocuments curryDoc uriPath (cMod, cFct, cTyp) =
  let cModState  = ixDoc
                   contextsMod
                   [moduleInfo curryDoc]
                   [doc uriPath mName (ModuleURI mName) (moduleInfo curryDoc)]
                   cMod
      cFctState  = ixDoc
                   contextsF
                   (functionInfos curryDoc)
                   (map (doc uriPath fName (FctOrTypeURI fModule fName))
                        $ functionInfos curryDoc)
                   cFct
      cTypeState = ixDoc
                   contextsT
                   (typeInfos curryDoc)
                   (map (doc uriPath tName (FctOrTypeURI tModule tName))
                        $ typeInfos curryDoc)
                   cTyp
  in (cModState, cFctState, cTypeState)

-- | Generates the context information for a module.
contextsMod :: ModuleInfo -> DocId -> [(String, String, Occurrences)]
contextsMod moduleI i =
  map (addOcc  (occ i 1)) $ [("module", mName moduleI)]
   ++ (author $ mAuthor moduleI)
   ++ (description $ mDescription moduleI)

-- | Generates the context information for a function.
contextsF :: FunctionInfo -> DocId -> [(String, String, Occurrences)]
contextsF functionI i =
  map (addOcc  (occ i 2)) $ [("function", fName functionI)]
   ++ [("inModule", fModule functionI)]
   ++ (signature $ signatureComponents $ fSignature functionI)
   ++ (flexRigid $ fFlexRigid functionI)
   ++ (nonDet $ fNonDet functionI)
   ++ (description $ fDescription functionI)
 where flexRigid fr = case fr of
                      KnownFlex  -> [("flexible", "")]
                      KnownRigid -> [("rigid"   , "")]
                      ConflictFR -> [("flexible", ""), ("rigid", "")]
                      _          -> []
       nonDet nd    = if nd then [("nondet", "")] else [("det", "")]

-- | Generates the context information for a type.
contextsT :: TypeInfo -> DocId -> [(String, String, Occurrences)]
contextsT typeI i =
  let sigPair = constrTypeExpr typeI
  in map (addOcc  (occ i 3)) $ [("type", tName typeI)]
      ++ [("inModule", tModule typeI)]
      ++ (signature (concatMap (signatureComponents . snd) (constrTypeExpr typeI)))
      ++ (functionContext $ map fst sigPair)
      ++ (description $ tDescription typeI)

-- Returns the maxId of a given document
maxId :: Binary a => SmallDocuments a -> DocId
maxId cDoc = maxKeyDocIdMap $ idToSmallDoc cDoc

-- Shortcut for docTable2smallDocTable conversion
docTable :: Binary a => HolumbusState a -> SmallDocuments a
docTable = docTable2smallDocTable . ixs_documents

-- When merging, ids have to be disjoint, so start ids with the increment of the other states maxId
editDoc :: Binary a => SmallDocuments a -> HolumbusState a -> SmallDocuments a
editDoc state2 state = editDocIds (addDocId $ maxId state2) $ docTable state

-- Adjusts the ids of the new created pair of index and documents, when merging
editIndex :: Binary a => SmallDocuments a -> HolumbusState a -> CompactInverted
editIndex docs state = inverted2compactInverted
                     $ updateDocIds' (addDocId (maxId docs)) (ixs_index state)

-- Union of a saved docIndex with a fresh one (ids have to be edited)
cUnion :: Binary a => SmallDocuments a -> CompactInverted -> HolumbusState a
                   -> (SmallDocuments a, CompactInverted)
cUnion cDoc cIndex state =
  unionDocIndex cDoc cIndex (editDoc cDoc state) (editIndex cDoc state)

-- Reorganizes files after creating temporary pair of index and document (due to merging)
reorganizeFiles :: IO ()
reorganizeFiles = do
  mapM_ reorganize [_moduleIndexPath, _functionIndexPath, _typeIndexPath]
  renameFile (listExtension (_moduleListPath ++ _tempFile))
             (listExtension  _moduleListPath)
  where
  reorganize path = do
    renameFile (indexExtension    (path ++ _tempFile))
               (indexExtension    path)
    renameFile (documentExtension (path ++ _tempFile))
               (documentExtension path)

-- When updating an index, temporary files have to be written
writeDocIndex :: Binary a => FilePath -> SmallDocuments a -> CompactInverted
              -> IO ()
writeDocIndex path cDoc cIndex = do
  putStr "."
  writeBin (indexExtension    path') cIndex
  writeBin (documentExtension path') cDoc
  where path' = path ++ _tempFile

-- | Writes and merges an existing pair of index and documents with a new one.
mergeIdxDoc :: [String] -> IO CurryIndex -> CurryIndexerStates -> IO ()
mergeIdxDoc moduleList ixDoc1 (cMod, cFct, cTyp) = do
  CurryIndex iM1 m1 iF1 f1 iT1 t1 <- ixDoc1
  currentModList                  <- loadModuleList
  let (mDoc, mIndex) = cUnion m1 iM1 cMod
      (fDoc, fIndex) = cUnion f1 iF1 cFct
      (tDoc, tIndex) = cUnion t1 iT1 cTyp
  writeDocIndex _moduleIndexPath   mDoc mIndex
  writeDocIndex _functionIndexPath fDoc fIndex
  writeDocIndex _typeIndexPath     tDoc tIndex
  writeFile (listExtension (_moduleListPath++_tempFile))
            (show $ moduleList ++ currentModList)
  reorganizeFiles

-- Loads the list of modules that exist in the index
loadModuleList :: IO [String]
loadModuleList = readFromFile (listExtension _moduleListPath)
