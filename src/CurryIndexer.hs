{- |
Module      :  CurryIndexer
Description :  Creates an index for the given data
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

This module creates the index and documents for a (or more) given cdoc file(s). 
Three pairs of index and documents are stored: for the module, function and type information.
-}

module Main (main) where 

import Data.Binary              (Binary (..), Word32, decodeFile)
import Data.Text as T           (splitOn, unpack, pack)
import Data.Text.IO as TIO      (readFile)

import System.Environment       (getArgs)
import System.Directory         (renameFile, getDirectoryContents)
import System.FilePath.Posix    (addTrailingPathSeparator, dropExtension, takeExtension, takeBaseName)

import CurryInfo
import FilesAndLoading
import Helpers (biasedWord, splitStringOn, splitOnWhitespace, showTypeList, 
                signatureComponents, varIndex)
import IndexTypes

import Holumbus.Index.CompactSmallDocuments (docTable2smallDocTable, idToSmallDoc)
import Holumbus.Index.Common.Occurences
import Holumbus.Index.Common.DocId (DocId, addDocId)
import Holumbus.Index.Common.DocIdMap (maxKeyDocIdMap)
import Holumbus.Index.Common (HolIndex (..), HolDocuments (..), HolDocIndex (..), Document (..))

_howToUseMessage :: String
_howToUseMessage = 
  "\n"++"No no no, you don't use it the right way. Let me give you a hint. \n"++
  "Try it like this: curryIndexer <cdoc_directory> <documentation_uri> [--n|--u]\n" ++
  "Or maybe like this curryIndexer " ++
  "<file_path_with_pairs_of_cdoc_directory_and_documentation_uri_sperated_with_;> [--n|--u]\n" ++
  "Inwhich '--u' updates your current index with the new data and " ++ 
  "'--n' creates a new index with the given data.\n"

-- Singleton shortcut for Occurences
occ :: DocId -> Word32 -> Occurrences
occ = singletonOccurrence

-- Adds an occurences to a tuple, returns a triple
addOcc :: Occurrences -> (a,b) -> (a,b,Occurrences)
addOcc occurrence (a,b) = (a,b,occurrence)

-- Adds description context to a string
description :: String -> [(String,String)]
description s = 
  map (addContext "description") $ filter (not . biasedWord) $ splitOnWhitespace s

-- Adds signature context to a signature string and all its suffixes
signature :: [String] -> [(String,String)]
signature = map (addContext "signature")

-- Adds author context to a string
author :: String -> [(String, String)]
author a = map (addContext "author") $ splitOnWhitespace a

-- Returns tupel of two strings
addContext :: String -> String -> (String, String)
addContext context s = (context, s)

-- Is a given module in the moduleList?
writeOrPass :: String -> IO CurryIndexerStates -> IO CurryIndexerStates
writeOrPass moduleName ioAction = do 
  mList <- loadModuleList
  if moduleName `elem` mList then return (emptyCurryModState, emptyCurryFctState, emptyCurryTypeState) 
                             else ioAction

-- Checks, if the module exists in the index
occurenceCheck :: Bool -> FilePath -> FilePath -> IO CurryIndexerStates -> IO CurryIndexerStates
occurenceCheck new uriPath cdocPath states = 
  if new then index
         else writeOrPass name index
 where name  = takeBaseName cdocPath
       index = prepareIndex uriPath cdocPath states


-- Helper function for writing index
prepareIndex :: FilePath -> FilePath -> IO CurryIndexerStates -> IO CurryIndexerStates
prepareIndex uriPath cdocPath states = do
  (cMod, cFct, cTyp) <- states
  curryDoc <- loadFromCurryFile cdocPath
  putStr "."
  return $ indexAndDocuments curryDoc uriPath (cMod, cFct, cTyp)

-- | Function to build a document.
doc :: Binary a => String -> (a -> String) -> URI a -> a -> Document a
doc uriPath fiName uriType info = 
  Document { title  = fiName info
           , uri    = uriP
           , custom = Just info
           }
 where uriP = case uriType of
                ModuleURI baseUri -> 
                  uriPath ++ baseUri info ++ ".html"
                FctOrTypeURI baseUri name -> 
                  uriPath ++ baseUri info ++ ".html" ++ "#" ++ name info

-- | Data to represent uri,
-- functions and types use anchors, i.e. moduleName.html#functionName.
data URI a = 
  ModuleURI (a -> String) | 
  FctOrTypeURI (a -> String) (a -> String)

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
                      KnownRigid -> [("rigid", "")]
                      ConflictFR -> [("flexible", ""), ("rigid", "")]
                      _          -> []
       nonDet nd    = if nd then [("nondet", "")] else [("det", "")]

-- | Generates the context information for a type
contextsT :: TypeInfo -> DocId -> [(String, String, Occurrences)]
contextsT typeI i = 
  let sigPair = map (showTypeList (tName typeI ++ (varIndex $ tVarIndex typeI))) 
                 $ tSignature typeI
  in map (addOcc  (occ i 3)) $ [("type", tName typeI)] 
      ++ [("inModule", tModule typeI)]
      ++ (concatMap signature $ map fst sigPair)
      ++ (signature $ map snd sigPair)
      ++ (description $ tDescription typeI)   

-- Returns the maxId of a given document
maxId :: Binary a => SmallDocuments a -> DocId
maxId cDoc = maxKeyDocIdMap $ idToSmallDoc cDoc

-- Shortcut for docTable2smallDocTable conversion
docTable :: Binary a => HolumbusState a -> SmallDocuments a
docTable = docTable2smallDocTable . ixs_documents

-- When merging, ids have to be disjoint, so start ids with the increment of the other states maxId
editDoc :: Binary a => SmallDocuments a -> HolumbusState a -> SmallDocuments a
editDoc state2 state = editDocIds (addDocId $ maxId state2) 
                                          $ docTable state
-- Adjusts the ids of the new created pair of index and documents, when merging
editIndex :: Binary a => SmallDocuments a -> HolumbusState a -> CompactInverted
editIndex docs state = 
  inverted2compactInverted $ updateDocIds' (addDocId (maxId docs)) 
                                           (ixs_index state)

-- Union of a saved docIndex with a fresh one (ids have to be edited)
cUnion :: Binary a => SmallDocuments a -> CompactInverted -> HolumbusState a
                   -> (SmallDocuments a, CompactInverted)
cUnion cDoc cIndex state = 
  unionDocIndex cDoc cIndex (editDoc cDoc state) (editIndex cDoc state)

-- Reorganizes files after creating temporary pair of index and document (due to merging)
reorganizeFiles :: IO ()
reorganizeFiles = do
    renameFile (indexExtension (_moduleIndexPath++_tempFile)) 
               (indexExtension _moduleIndexPath) 
    renameFile (documentExtension (_moduleIndexPath++_tempFile)) 
               (documentExtension _moduleIndexPath)
    renameFile (indexExtension (_functionIndexPath++_tempFile)) 
               (indexExtension _functionIndexPath) 
    renameFile (documentExtension (_functionIndexPath++_tempFile)) 
               (documentExtension _functionIndexPath)
    renameFile (indexExtension (_typeIndexPath++_tempFile)) 
               (indexExtension _typeIndexPath) 
    renameFile (documentExtension (_typeIndexPath++_tempFile)) 
               (documentExtension _typeIndexPath)
    renameFile (listExtension (_moduleListPath++_tempFile)) 
               (listExtension _moduleListPath)

-- When updating an index, temporary files have to be written
writeDocIndex :: Binary a => FilePath -> SmallDocuments a -> CompactInverted -> IO ()
writeDocIndex path cDoc cIndex = do
  putStr "."
  writeBin (indexExtension (path++_tempFile)) cIndex
  writeBin (documentExtension (path++_tempFile)) cDoc

-- | Writes and merges an existing pair of index and documents with a new one.
mergeIdxDoc :: [String] -> IO LoadedIndexerStates -> CurryIndexerStates -> IO ()
mergeIdxDoc moduleList ixDoc1 (cMod, cFct, cTyp) = do
  ((iM1, m1), (iF1, f1), (iT1, t1)) <- ixDoc1
  currentModList                    <- loadModuleList
  let (mDoc, mIndex) = cUnion m1 iM1 cMod
      (fDoc, fIndex) = cUnion f1 iF1 cFct
      (tDoc, tIndex) = cUnion t1 iT1 cTyp
  writeDocIndex _moduleIndexPath mDoc mIndex
  writeDocIndex _functionIndexPath fDoc fIndex
  writeDocIndex _typeIndexPath tDoc tIndex  
  writeFile (listExtension (_moduleListPath++_tempFile)) (show $ moduleList ++ currentModList)
  reorganizeFiles

-- Shortcut to load a pair of index and documents
loadIdxDoc :: Binary a => FilePath -> IO (LoadedIndexerState a)
loadIdxDoc fPath = do
  index <- decodeFile (indexExtension fPath)
  docs  <- loadDocuments (documentExtension fPath)
  return (index, docs)
  
-- Loads pair of index and documents for module, function and type
loadIndexerStates :: IO LoadedIndexerStates
loadIndexerStates = do 
  modState <- loadIdxDoc _moduleIndexPath
  fctState <- loadIdxDoc _functionIndexPath
  typState <- loadIdxDoc _typeIndexPath
  return (modState, fctState, typState)

-- Loads the list of modules that exist in the index
loadModuleList :: IO [String]
loadModuleList = do --loadFromBinFile moduleListPath
  text <- Prelude.readFile (listExtension _moduleListPath)
  list <- readIO text 
  return list

-- Writes index and documents with a function that converts the types automatically 
-- (Inverted to CompactInverted, Documents a to SmallDocuments a)
writeIndex' :: [String] -> (CurryModIndexerState, CurryFctIndexerState, CurryTypeIndexerState) -> IO ()
writeIndex' moduleList (curryModState, curryFctState, curryTypeState) = do
  putStr "."
  writeSearchBin _moduleIndexPath curryModState
  putStr "."
  writeSearchBin _functionIndexPath curryFctState   
  putStr "."
  writeSearchBin _typeIndexPath curryTypeState
  writeFile (listExtension _moduleListPath) (show moduleList)

-- | Writes a new index when flag is true, merges new index with existing one otherwise
writeIndex :: Bool -> FilePath -> [FilePath] -> IO ()
writeIndex _ _ [] =
  putStr "There are no cdoc-Files in this directory" 
writeIndex new uriPath files = do
  (nM,nF,nT) <- foldr (occurenceCheck new uriPath) emptyStates files
  if new then writeIndex' moduleNames (nM,nF,nT)
         else mergeIdxDoc moduleNames loadIndexerStates (nM, nF, nT)  
 where emptyStates = return (emptyCurryModState, emptyCurryFctState, emptyCurryTypeState)
       moduleNames = map (dropExtension . last . splitStringOn "/") files

-- Reads txt-file with pairs of cdoc and uri paths that are seperated by ';'
-- to create or update (new flag) the index
readFilePaths :: Bool -> FilePath -> IO ()
readFilePaths new fPath = do
  file <- TIO.readFile fPath
  prepareFilePaths new $ filePathList file
 where pairs = T.splitOn (T.pack "\n")
       filePathList = map T.unpack . concatMap (T.splitOn $ T.pack ";") . pairs
    
-- Main function to start the index creation
startIndexer :: Bool -> FilePath -> FilePath -> IO ()
startIndexer new cdocP uriP = do
  putStr "Writing index..."
  files <- getDirectoryContents cdocP >>= return . filter (\c -> takeExtension c == ".cdoc")
  writeIndex new uriPath $ map (cdocPath ++) files
  putStr "done!\n"
 where uriPath  = addTrailingPathSeparator uriP
       cdocPath = addTrailingPathSeparator cdocP

-- Recursive function to start indexing,
-- either a new index is created or the existing index is updated (new flag)
prepareFilePaths :: Bool -> [FilePath] -> IO ()
prepareFilePaths new (cdocPath:uriPath:xs) = 
  startIndexer new cdocPath uriPath >> prepareFilePaths False xs
prepareFilePaths _ [""] = return ()
prepareFilePaths _ _    = putStr _howToUseMessage

-- Analyzes arguments of the command line
processArgs :: [String] -> IO ()
processArgs args =
  case args of 
   [fPath,"--n"]            -> readFilePaths True fPath
   [fPath,"--u"]            -> readFilePaths False fPath
   [cdocPath,uriPath,"--n"] -> startIndexer True cdocPath uriPath
   [cdocPath,uriPath,"--u"] -> startIndexer False cdocPath uriPath    
   _                        -> putStr _howToUseMessage

main :: IO ()
main = getArgs >>= processArgs