{- |
Module      :  CurryIndexer
Description :  Creates an index for the given data
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

This module 
-}

module Main (main) where 

import Control.Monad (void)

import Data.Binary
import Data.List

import System.Environment (getArgs)
import System.Directory (renameFile, getDirectoryContents)
import System.FilePath.Posix

import CurryInfo
import FilesAndLoading
import Helpers (splitOnWhitespace, biasedWord, typeSignature, consSignature)
import IndexTypes

import Holumbus.Index.CompactSmallDocuments
import Holumbus.Index.Common
import Holumbus.Crawler.IndexerCore (IndexerState (..))

import Data.Text as T (splitOn, unpack, pack, concat)
import Data.Text.IO as TIO (readFile)

main :: IO ()
main = do
     args <- getArgs
     processArgs args

processArgs :: [String] -> IO ()
processArgs args =
    case args of 
         [filePath,"--n"]          -> readFilePaths True filePath
         [filePath,"--u"]          -> readFilePaths False filePath
         [cdocPath,uriPath,"--n"]  -> main2 True cdocPath uriPath
         [cdocPath,uriPath,"--u"]  -> main2 False cdocPath uriPath
         -- filePaths@(_:_)           -> prepareFilePaths True filePaths
         _                         -> putStr howToUseMessage

prepareFilePaths :: Bool -> [FilePath] -> IO ()
prepareFilePaths new (cdocPath:uriPath:xs) = main2 new cdocPath uriPath >> prepareFilePaths False xs
prepareFilePaths new [""]                    = return ()
prepareFilePaths _ _                       = putStr howToUseMessage

readFilePaths :: Bool -> FilePath -> IO ()
readFilePaths new filePath = do
    lines <- TIO.readFile filePath
    let lines2 = T.splitOn (T.pack "\n") lines 
    print $ concatMap (T.splitOn (T.pack ";")) lines2
    prepareFilePaths new $ map T.unpack (concatMap (T.splitOn (T.pack ";")) lines2)

    

howToUseMessage :: String
howToUseMessage = 
    "\n"++"No no no, you don't use it the right way. Let me give you a hint. \n"++
    "Try it like this: curryIndexer <cdoc_directory> <documentation_uri> [--n|--u]\n" ++
    "Inwhich '--u' updates your current index with the new data and '--n' creates a new index with the given data.\n"


main2 :: Bool -> FilePath -> FilePath -> IO ()
main2 new cdocP uriP = do
    putStr "Writing index..."
    files <- getDirectoryContents cdocP >>= return . filter (\c -> takeExtension c == ".cdoc")
    writeIndex new uriPath $ map (cdocPath ++) files
    putStr "done!\n"
  where fullPath path  = if last path == '/' then path else path ++ "/"
        uriPath        = fullPath uriP
        cdocPath       = fullPath cdocP
        -- directoryFiles = map (cdocPath ++) files

--  Takes an uriPath and a list of files
-- | Writes a new index when flag is true, merges new index with existing one otherwise
writeIndex :: Bool -> FilePath -> [FilePath] -> IO ()
writeIndex _ _ [] =
    putStr "There are no cdoc-Files in this directory" 
writeIndex new uriPath files = do
   let emptyStates = (emptyCurryModState, emptyCurryFctState, emptyCurryTypeState)
   (nM,nF,nT) <- foldr (prepareIndex uriPath) (return emptyStates) files
   if new then writeIndex' (nM,nF,nT) 
          else mergeIdxDoc (indexerStates (moduleIndex, functionIndex, typeIndex)) (nM, nF, nT)

type MergeIdxDoc =  ((CompactInverted, SmallDocuments ModuleInfo), 
                   (CompactInverted, SmallDocuments FunctionInfo), 
                   (CompactInverted, SmallDocuments TypeInfo))

mergeIdxDoc :: IO MergeIdxDoc -> (CurryModIndexerState, 
                                  CurryFctIndexerState, 
                                  CurryTypeIndexerState) -> IO ()
mergeIdxDoc ixDoc1 (cMod, cFct, cTyp) = do
    ((iM1, m1), (iF1, f1), (iT1, t1)) <- ixDoc1
    let (mDoc, mIndex) = unionDocIndex m1 iM1 (editDoc m1 cMod) (editIndex cMod m1)
    let (fDoc, fIndex) = unionDocIndex f1 iF1 (editDoc f1 cFct) (editIndex cFct f1)
    let (tDoc, tIndex) = unionDocIndex t1 iT1 (editDoc t1 cTyp) (editIndex cTyp t1)
    writeDocIndex moduleIndex mDoc mIndex
    writeDocIndex functionIndex fDoc fIndex
    writeDocIndex typeIndex tDoc tIndex     

editIndex :: (Binary a) => HolumbusState a -> SmallDocuments a -> CompactInverted
editIndex state state2 = 
          inverted2compactInverted $ updateDocIds' (addDocId (maxId state2)) 
                                                   (ixs_index state)

-- docTable2smallDocTable shortcut
docTable :: (Binary a) => HolumbusState a -> SmallDocuments a
docTable = docTable2smallDocTable . ixs_documents

-- returns the maxId of a given document
maxId :: (Binary a) => SmallDocuments a -> DocId
maxId cDoc = maxKeyDocIdMap $ idToSmallDoc cDoc

-- when merging, ids have to be disjoint, so start ids with the increment of the other states maxId
editDoc :: (Binary a) => SmallDocuments a -> HolumbusState a -> SmallDocuments a
editDoc state2 state = editDocIds (addDocId $ maxId state2) 
                                          $ docTable state
union :: (Binary a) => SmallDocuments a -> CompactInverted -> HolumbusState a -> 
                       (SmallDocuments a, CompactInverted)
union cDoc cIndex state = 
  unionDocIndex cDoc cIndex (editDoc cDoc state) (editIndex state cDoc)
        
writeDocIndex :: (Binary a) => FilePath -> SmallDocuments a -> CompactInverted -> IO ()
writeDocIndex path cDoc cIndex = do
    putStr "."
    writeBin (path++"2.idx") cIndex
    writeBin (path++"2.doc") cDoc
    reorganizeFiles path

reorganizeFiles :: FilePath -> IO ()
reorganizeFiles path = do
    renameFile (path++"2.idx") (path++".idx") 
    renameFile (path++"2.doc") (path++".doc")

writeIndex' :: (CurryModIndexerState, CurryFctIndexerState, CurryTypeIndexerState) -> IO ()
writeIndex' (curryModState, curryFctState, curryTypeState) = do
    putStr "."
    writeSearchBin moduleIndex curryModState
    putStr "."
    writeSearchBin functionIndex curryFctState
    putStr "."
    writeSearchBin typeIndex curryTypeState

    
indexerStates :: (FilePath, FilePath, FilePath) -> IO ((CompactInverted, SmallDocuments ModuleInfo), 
                                                       (CompactInverted, SmallDocuments FunctionInfo), 
                                                       (CompactInverted, SmallDocuments TypeInfo))
indexerStates (mp, fp, tp) = do 
       cMod  <- loadModIdxDoc mp
       cFct  <- loadFctIdxDoc fp
       cTyp  <- loadTypeIdxDoc tp
       return (cMod, cFct, cTyp)

-- Helper function for writing index
prepareIndex :: FilePath -> FilePath -> IO (CurryModIndexerState, 
                                         CurryFctIndexerState, 
                                         CurryTypeIndexerState) -> IO (CurryModIndexerState, 
                                                                          CurryFctIndexerState, 
                                                                          CurryTypeIndexerState)
prepareIndex uriPath cdocPath states = do
    (cMod, cFct, cTyp) <- states
    curryDoc <- loadFromCurryFile cdocPath
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
    putStr "."
    return (curryModState, curryFctState, curryTypeState)


------------------------------------

-- short names for loading file paths
loadModIdxDoc :: FilePath ->  IO (CompactInverted, SmallDocuments ModuleInfo)
loadModIdxDoc fp = do
    index <- decodeFile (fp++".idx") 
    dcmts <- loadModDocuments (fp++".doc")
    return (index, dcmts)

loadFctIdxDoc :: FilePath ->  IO (CompactInverted, SmallDocuments FunctionInfo)
loadFctIdxDoc fp = do
    index <- decodeFile (fp++".idx") 
    dcmts <- loadFctDocuments (fp++".doc")
    return (index, dcmts)

loadTypeIdxDoc :: FilePath ->  IO (CompactInverted, SmallDocuments TypeInfo)
loadTypeIdxDoc fp = do
    index <- decodeFile (fp++".idx") 
    dcmts <- loadTypeDocuments (fp++".doc")
    return (index, dcmts)

moduleIndex :: FilePath
moduleIndex = "./index/ix-mod.bin"
-- moduleIndex2 :: FilePath
-- moduleIndex2 = "./index/ix-mod.bin2"

functionIndex :: FilePath
functionIndex = "./index/ix-fct.bin"
-- functionIndex2 :: FilePath
-- functionIndex2 = "./index/ix-fct.bin2"

typeIndex :: FilePath
typeIndex = "./index/ix-type.bin"
-- typeIndex2 :: FilePath
-- typeIndex2 = "./index/ix-type.bin2"
------------------------------------

-- |  Main indexer method to build indexes and documents
ixDoc :: (Binary a) => (a -> DocId -> [(String, String, Occurrences)]) -> 
                       [a] -> [Document a] -> 
                       IndexerState Inverted Documents a -> 
                       IndexerState Inverted Documents a
ixDoc contextList (info:infos) (doc1:docs) (IndexerState ix dc) = 
    let (docId, docs') = insertDoc dc doc1
        idx'           = mergeIndexes ix $ idx contextList info docId
    in ixDoc contextList infos docs (IndexerState idx' docs')
ixDoc _ _ _ is = is

-- | Data to represent uri
-- functions and types use anchors, i.e. moduleName.html#functionName
data Uri a = ModuleUri (a -> String) | FctOrTypeUri (a -> String) (a -> String)

-- | Function to build document
doc :: (Binary a) => String -> (a -> String) -> Uri a -> a -> Document a
-- doc uriPath fiName (ModuleUri fiUri) info = 
--     Document {title  = fiName info
--              ,uri    = uriPath ++ fiUri info ++ ".html"
--              ,custom = Just info}
-- doc uriPath fiName (FctOrTypeUri fiUriModule fiUriName) info = 
--     Document {title  = fiName info
--              ,uri    = uriPath ++ fiUriModule info ++ ".html" ++ 
--                        "#" ++ fiUriName info
--              ,custom = Just info} 
doc uriPath fiName uriType info = 
    Document {title  = fiName info,
              uri    = uriP,
              custom = Just info}
  where uriP = case uriType of
               ModuleUri fiUri                    -> uriPath ++ fiUri info ++ ".html"
               FctOrTypeUri fiUriModule fiUriName -> uriPath ++ fiUriModule info ++ ".html" ++
                                                      "#" ++ fiUriName info

-- | Function to build index
idx :: (a -> DocId -> [(String, String, Occurrences)]) -> a -> DocId -> Inverted
idx contextList info i = fromList emptyInverted $ contextList info i

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
                              ++ (signature $ (\((modName,_), tExpr) -> typeSignature modName tExpr) 
                                  $ fSignature functionI)
                              ++ (flexRigid $ fFlexRigid functionI)
                              ++ (nonDet $ fNonDet functionI)
                              ++ (description $ fDescription functionI) 
  where flexRigid fr = case fr of
                       KnownFlex  -> [("Flex", "")]     
                       KnownRigid -> [("Rigid", "")]
                       ConflictFR -> [("Flex", ""), ("Rigid", "")]
                       _          -> []
        nonDet nd 
          | True      = [("NonDet", "")]
          | otherwise = [("Det", "")]

-- | Generates the context information for a type
contextsT :: TypeInfo -> DocId -> [(String, String, Occurrences)]
contextsT typeI i = 
    map (addOcc  (occ i 1)) $ [("Type", tName typeI)] ++ [("Module", tModule typeI)]
                              ++ (signature (concatMap 
                                 (\((modName,_), tExprList) -> consSignature modName tExprList)
                                 $ tSignature typeI))
                              ++ (signature $ map (\((_, fctName), _) -> fctName) $ tSignature typeI)
                              ++ (description $ tDescription typeI)       

occ :: DocId -> Word32 -> Occurrences
occ = singletonOccurrence

addOcc :: Occurrences -> (a,b) -> (a,b,Occurrences)
addOcc occurrence (a,b) = (a,b,occurrence)

description :: String -> [(String,String)]
description s = map (addContext "Description") $ filter (not . biasedWord) $ splitOnWhitespace s

signature :: [String] -> [(String,String)]
signature s = addContext "Signature" (intercalate "->" s) : map (addContext "Signature") s

addContext :: String -> String -> (String, String)
addContext context s = (context, s)

author :: String -> [(String, String)]
author a = map (addContext "Author") $ splitOnWhitespace a