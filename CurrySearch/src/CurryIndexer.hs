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

import Data.Binary
import Data.List
import Data.Text as T           (splitOn, unpack, pack)
import Data.Text.IO as TIO      (readFile)

import System.Environment       (getArgs)
import System.Directory         (renameFile, getDirectoryContents)
import System.FilePath.Posix

import CurryInfo
import FilesAndLoading
import Helpers
import IndexTypes

import Holumbus.Index.CompactSmallDocuments
import Holumbus.Index.Common

import Debug.Trace (trace)

-- singleton shortcut
occ :: DocId -> Word32 -> Occurrences
occ = singletonOccurrence

-- take a tupel and an Occurencces-data and returns a tripel
addOcc :: Occurrences -> (a,b) -> (a,b,Occurrences)
addOcc occurrence (a,b) = (a,b,occurrence)

-- adds description context to a string
description :: String -> [(String,String)]
description s = map (addContext "Description") $ filter (not . biasedWord) $ splitOnWhitespace s

-- adds signature context to a string
signature :: [String] -> [(String,String)]
signature = map (addContext "Signature") . init . map listToSignature . tails

-- adds author context to a string
author :: String -> [(String, String)]
author a = map (addContext "Author") $ splitOnWhitespace a

-- returns tupel of two strings
addContext :: String -> String -> (String, String)
addContext context s = (context, s)

-- Helper function for writing index
prepareIndex :: FilePath -> FilePath -> IO CurryIndexerStates -> IO CurryIndexerStates
prepareIndex uriPath cdocPath states = do
    (cMod, cFct, cTyp) <- states
    curryDoc <- loadFromCurryFile cdocPath
    putStr "."
    return $ indexAndDocuments curryDoc uriPath (cMod, cFct, cTyp)

-- | Function to build document
doc :: Binary a => String -> (a -> String) -> Uri a -> a -> Document a
doc uriPath fiName uriType info = Document {title  = fiName info,
                                            uri    = uriP,
                                            custom = Just info}
  where uriP = case uriType of
               ModuleUri fiUri                    -> uriPath ++ fiUri info ++ ".html"
               FctOrTypeUri fiUriModule fiUriName -> uriPath ++ fiUriModule info ++ ".html"
                                                             ++ "#" ++ fiUriName info

-- | Data to represent uri
-- functions and types use anchors, i.e. moduleName.html#functionName
data Uri a = ModuleUri (a -> String) | FctOrTypeUri (a -> String) (a -> String)

-- | Function to build index
idx :: (a -> DocId -> [(String, String, Occurrences)]) -> a -> DocId -> Inverted
idx contextList info i = fromList emptyInverted $ contextList info i
    
-- |  Main indexer method to build indexes and documents
ixDoc :: Binary a => (a -> DocId -> [(String, String, Occurrences)]) -> 
                       [a] -> [Document a] -> HolumbusState a -> HolumbusState a
ixDoc contextList (info:infos) (doc1:docs) (IndexerState ix dc) = 
    let (docId, docs') = insertDoc dc doc1
        idx'           = mergeIndexes ix $ idx contextList info docId
    in ixDoc contextList infos docs (makeIndexerState idx' docs')
ixDoc _ _ _ is = is

-- calls ixDoc to create index and documents for the module, functions and types
indexAndDocuments :: CurryInfo -> FilePath  -> CurryIndexerStates -> CurryIndexerStates
indexAndDocuments curryDoc uriPath (cMod, cFct, cTyp) = 
  let curryModState  = ixDoc 
                       contextsMod 
                       [moduleInfo curryDoc] 
                       [doc uriPath mName (ModuleUri mName) (moduleInfo curryDoc)] 
                       cMod
      curryFctState  = ixDoc 
                       contextsF 
                       (functionInfos curryDoc) 
                       (map (doc uriPath fName (FctOrTypeUri fModule fName)) $ functionInfos curryDoc)
                       cFct
      curryTypeState = ixDoc 
                       contextsT 
                       (typeInfos curryDoc) 
                       (map (doc uriPath tName (FctOrTypeUri tModule tName)) $ typeInfos curryDoc) 
                       cTyp
  in (curryModState, curryFctState, curryTypeState)

-- | Generates the context information for a module
contextsMod :: ModuleInfo -> DocId -> [(String, String, Occurrences)]
contextsMod moduleI i = 
    map (addOcc  (occ i 1)) $ [("TheModule", mName moduleI)] 
                                   ++ (author $ mAuthor moduleI)
                                   ++ (description $ mDescription moduleI)

-- | Generates the context information for a function
contextsF :: FunctionInfo -> DocId -> [(String, String, Occurrences)]
contextsF functionI i =
    map (addOcc  (occ i 2)) $ [("Function", fName functionI)] ++ [("Module", fModule functionI)]
                              ++ trace (show $ (signature $ (\((modName,_), tExpr) -> signatureList modName tExpr) 
                                  $ fSignature functionI)) ((signature $ (\((modName,_), tExpr) -> signatureList modName tExpr) 
                                  $ fSignature functionI))
                              ++ (flexRigid $ fFlexRigid functionI)
                              ++ (nonDet $ fNonDet functionI)
                              ++ (description $ fDescription functionI) 
  where flexRigid fr = case fr of
                       KnownFlex  -> [("Flex", "")]     
                       KnownRigid -> [("Rigid", "")]
                       ConflictFR -> [("Flex", ""), ("Rigid", "")]
                       _          -> []
        nonDet nd    = if nd then [("NonDet", "")] else [("Det", "")]

-- | Generates the context information for a type
contextsT :: TypeInfo -> DocId -> [(String, String, Occurrences)]
contextsT typeI i = 
    map (addOcc  (occ i 1)) $ [("Type", tName typeI)] ++ [("Module", tModule typeI)]
                              ++ (signature (concatMap 
                                 (\((modName,_), tExprList) -> consSignature modName tExprList)
                                 $ tSignature typeI))
                              ++ (signature $ map (\((_, fctName), _) -> fctName) $ tSignature typeI)
                              ++ (description $ tDescription typeI)   

-- returns the maxId of a given document
maxId :: Binary a => SmallDocuments a -> DocId
maxId cDoc = maxKeyDocIdMap $ idToSmallDoc cDoc

-- docTable2smallDocTable shortcut
docTable :: Binary a => HolumbusState a -> SmallDocuments a
docTable = docTable2smallDocTable . ixs_documents

-- when merging, ids have to be disjoint, so start ids with the increment of the other states maxId
editDoc :: Binary a => SmallDocuments a -> HolumbusState a -> SmallDocuments a
editDoc state2 state = editDocIds (addDocId $ maxId state2) 
                                          $ docTable state
-- adjusts the ids of the new created pair of index and documents, when merging
editIndex :: Binary a => SmallDocuments a -> HolumbusState a -> CompactInverted
editIndex docs state = 
  inverted2compactInverted $ updateDocIds' (addDocId (maxId docs)) 
                                           (ixs_index state)

-- union of a saved docIndex with a fresh one (ids have to be edited)
cUnion :: Binary a => SmallDocuments a -> CompactInverted -> HolumbusState a
                   -> (SmallDocuments a, CompactInverted)
cUnion cDoc cIndex state = 
  unionDocIndex cDoc cIndex (editDoc cDoc state) (editIndex cDoc state)

-- reorganizes files after creating temporary pair of index and document (due to merging)
reorganizeFiles :: FilePath -> IO ()
reorganizeFiles path = do
    renameFile (indexExtension (path++tempFile)) (indexExtension path) 
    renameFile (documentExtension (path++tempFile)) (documentExtension path)

-- when updating an index, temporary files have to be written
writeDocIndex :: Binary a => FilePath -> SmallDocuments a -> CompactInverted -> IO ()
writeDocIndex path cDoc cIndex = do
    putStr "."
    writeBin (indexExtension (path++tempFile)) cIndex
    writeBin (documentExtension (path++tempFile)) cDoc
    reorganizeFiles path

-- | Writes and merges an existing pair of index and documents with a new one
mergeIdxDoc :: IO LoadedIndexerStates -> CurryIndexerStates -> IO ()
mergeIdxDoc ixDoc1 (cMod, cFct, cTyp) = do
  ((iM1, m1), (iF1, f1), (iT1, t1)) <- ixDoc1
  let (mDoc, mIndex) = cUnion m1 iM1 cMod
      (fDoc, fIndex) = cUnion f1 iF1 cFct
      (tDoc, tIndex) = cUnion t1 iT1 cTyp
  writeDocIndex moduleIndexPath mDoc mIndex
  writeDocIndex functionIndexPath fDoc fIndex
  writeDocIndex typeIndexPath tDoc tIndex   

-- shortcut to load a pair of index and documents
loadIdxDoc :: Binary a => FilePath -> IO (LoadedIndexerState a)
loadIdxDoc fPath = do
  index <- decodeFile (indexExtension fPath)
  docs  <- loadDocuments (documentExtension fPath)
  return (index, docs)
  
-- loads pair of index and documents for module, function and type
loadIndexerStates :: IO LoadedIndexerStates
loadIndexerStates = do 
  modState <- loadIdxDoc moduleIndexPath
  fctState <- loadIdxDoc functionIndexPath
  typState <- loadIdxDoc typeIndexPath
  return (modState, fctState, typState)

-- writes index and documents with a function that converts the types automatically 
-- (Inverted to CompactInverted, Documents a to SmallDocuments a)
writeIndex' :: (CurryModIndexerState, CurryFctIndexerState, CurryTypeIndexerState) -> IO ()
writeIndex' (curryModState, curryFctState, curryTypeState) = do
  putStr "."
  writeSearchBin moduleIndexPath curryModState
  putStr "."
  writeSearchBin functionIndexPath curryFctState   
  putStr "."
  writeSearchBin typeIndexPath curryTypeState  

-- | Writes a new index when flag is true, merges new index with existing one otherwise
writeIndex :: Bool -> FilePath -> [FilePath] -> IO ()
writeIndex _ _ [] =
  putStr "There are no cdoc-Files in this directory" 
writeIndex new uriPath files = do
  (nM,nF,nT) <- foldr (prepareIndex uriPath) emptyStates files
  if new then writeIndex' (nM,nF,nT) 
         else mergeIdxDoc loadIndexerStates (nM, nF, nT)  
 where emptyStates = return (emptyCurryModState, emptyCurryFctState, emptyCurryTypeState)

-- reads out txt-file with pairs of cdoc and uri paths that are seperated by ';'
-- to create or update (new flag) the index 
readFilePaths :: Bool -> FilePath -> IO ()
readFilePaths new fPath = do
  file <- TIO.readFile fPath
  prepareFilePaths new $ filePathList file
 where pairs = T.splitOn (T.pack "\n")
       filePathList = map T.unpack . concatMap (T.splitOn $ T.pack ";") . pairs

howToUseMessage :: String
howToUseMessage = 
  "\n"++"No no no, you don't use it the right way. Let me give you a hint. \n"++
  "Try it like this: curryIndexer <cdoc_directory> <documentation_uri> [--n|--u]\n" ++
  "Or maybe like this curryIndexer " ++
  "<file_path_with_pairs_of_cdoc_directory_and_documentation_uri_sperated_with_;> [--n|--u]\n" ++
  "Inwhich '--u' updates your current index with the new data and " ++ 
  "'--n' creates a new index with the given data.\n"
    
-- main function to start the index creation
main2 :: Bool -> FilePath -> FilePath -> IO ()
main2 new cdocP uriP = do
  putStr "Writing index..."
  files <- getDirectoryContents cdocP >>= return . filter (\c -> takeExtension c == ".cdoc")
  writeIndex new uriPath $ map (cdocPath ++) files
  putStr "done!\n"
 where uriPath        = fullPath uriP
       cdocPath       = fullPath cdocP

-- recursive function to start indexing
-- either a new index is created or the existing index is updated (new flag)
prepareFilePaths :: Bool -> [FilePath] -> IO ()
prepareFilePaths new (cdocPath:uriPath:xs) = main2 new cdocPath uriPath >> prepareFilePaths False xs
prepareFilePaths _ [""]                    = return ()
prepareFilePaths _ _                       = putStr howToUseMessage

-- analyzes arguments of the command line
processArgs :: [String] -> IO ()
processArgs args =
    case args of 
         [fPath,"--n"]             -> readFilePaths True fPath
         [fPath,"--u"]             -> readFilePaths False fPath
         [cdocPath,uriPath,"--n"]  -> main2 True cdocPath uriPath
         [cdocPath,uriPath,"--u"]  -> main2 False cdocPath uriPath
         _                         -> putStr howToUseMessage

main :: IO ()
main = do args <- getArgs
          processArgs args