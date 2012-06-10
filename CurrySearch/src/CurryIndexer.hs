module Main (main) 

where 

import Holumbus.Index.Common
import Holumbus.Crawler.IndexerCore (IndexerState (..))
import Data.Binary
import CurryInfo
import IndexTypes
import Helpers (splitOnWhitespace, biasedWord, listToSignature)


main :: IO ()
main = do
    putStr $ "Writing index ..."
    curryDoc <- loadFromCurryFile $ filePath ++ "/DOC_FirstProg/firstprog.cdoc"
    let curryModState  = ixDoc 
                         contextsMod 
                         [moduleInfo curryDoc] 
                         [doc mName (ModuleUri mName) (moduleInfo curryDoc)] 
                         emptyCurryModState
        curryFctState  = ixDoc 
                         contextsF 
                         (functionInfos curryDoc) 
                         (map (doc fName (FctOrTypeUri fModule fName)) $ functionInfos curryDoc)
                         emptyCurryFctState
        curryTypeState = ixDoc 
                         contextsT 
                         (typeInfos curryDoc) 
                         (map (doc tName (FctOrTypeUri tModule tName)) $ typeInfos curryDoc) 
                         emptyCurryTypeState
    putStr $ " done!\n"
    writeSearchBin "../index/ix-mod.bin"  $ curryModState
    writeSearchBin "../index/ix-fct.bin"  $ curryFctState
    writeSearchBin "../index/ix-type.bin" $ curryTypeState

-- |  Main indexer method to build indexes and documents
ixDoc :: (Binary a) => (a -> DocId -> [(String, String, Occurrences)]) -> 
                       [a] -> 
                       [Document a] -> 
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
doc :: (Binary a) => (a -> String) -> Uri a -> a -> Document a
doc fiName (ModuleUri fiUri) info = 
    Document {title  = fiName info
             ,uri    = "DOC_" ++ fiUri info ++ "/" ++ fiUri info ++ ".html"
             ,custom = Just info}
doc fiName (FctOrTypeUri fiUriModule fiUriName) info = 
    Document {title  = fiName info
             ,uri    = "DOC_" ++ fiUriModule info ++ "/" ++
                       fiUriModule info ++ ".html" ++ 
                       "#" ++ fiUriName info
             ,custom = Just info}
 
-- | Function to build index
idx :: (a -> DocId -> [(String, String, Occurrences)]) -> a -> DocId -> Inverted
idx contextList info i = fromList emptyInverted $ contextList info i

-- | Generates the context information for a module
contextsMod :: ModuleInfo -> DocId -> [(String, String, Occurrences)]
contextsMod moduleI i = 
    map (addOcc  (occ i 1)) $ [("Name", mName moduleI)] 
                                   ++ (author $ mAuthor moduleI) 
                                   ++ (description $ mDescription moduleI)

-- | Generates the context information for a function
contextsF :: FunctionInfo -> DocId -> [(String, String, Occurrences)]
contextsF functionI i =
    map (addOcc  (occ i 2)) $ [("Name", fName functionI)
                              , ("Module", fModule functionI) 
                              , ("Signature", listToSignature $ fSignature functionI)] 
                              ++ (description $ fDescription functionI)      
    
-- | Generates the context information for a type
contextsT :: TypeInfo -> DocId -> [(String, String, Occurrences)]
contextsT typeI i = 
    map (addOcc  (occ i 1)) $ [("Name", tName typeI)
                              , ("Module", tModule typeI)
                              , ("Signature", listToSignature $ concat $ tSignature typeI)]
                              ++ (description $ tDescription typeI)       

occ :: DocId -> Word32 -> Occurrences
occ dId i = singletonOccurrence dId i

addOcc :: Occurrences -> (a,b) -> (a,b,Occurrences)
addOcc occurrence (a,b) = (a,b,occurrence)

description :: String -> [(String,String)]
description s = map (addContext "Description") $ filter (not . biasedWord) $ splitOnWhitespace s

addContext :: String -> String -> (String, String)
addContext context s = (context, s)

author :: String -> [(String, String)]
author a = map (addContext "Author") $ splitOnWhitespace $ a