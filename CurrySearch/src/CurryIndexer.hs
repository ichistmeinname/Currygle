module Main (main) 

where 

import Holumbus.Index.Common
import Holumbus.Crawler.IndexerCore (IndexerState (..))
import Data.Binary
import Data.List
import CurryInfo
import IndexTypes
import Helpers (splitOnWhitespace, biasedWord, typeSignature, consSignature)
import System.Environment (getArgs)

main :: IO ()
main = do
     (a:b:_) <- getArgs
     main2 a b
     return ()

main2 :: FilePath -> FilePath -> IO ()
main2 cdocPath uriPath = do
    putStr $ "Writing index ..."
    curryDoc <- loadFromCurryFile $ cdocPath
    let curryModState  = ixDoc 
                         contextsMod 
                         [moduleInfo curryDoc] 
                         [doc uriPath mName (ModuleUri mName) (moduleInfo curryDoc)] 
                         emptyCurryModState
        curryFctState  = ixDoc 
                         contextsF 
                         (functionInfos curryDoc) 
                         (map (doc uriPath fName (FctOrTypeUri fModule fName)) $ functionInfos curryDoc)
                         emptyCurryFctState
        curryTypeState = ixDoc 
                         contextsT 
                         (typeInfos curryDoc) 
                         (map (doc uriPath tName (FctOrTypeUri tModule tName)) $ typeInfos curryDoc) 
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
    map (addOcc  (occ i 1)) $ [("Name", mName moduleI)] 
                                   ++ (author $ mAuthor moduleI) 
                                   ++ (description $ mDescription moduleI)

-- | Generates the context information for a function
contextsF :: FunctionInfo -> DocId -> [(String, String, Occurrences)]
contextsF functionI i =
    map (addOcc  (occ i 2)) $ [("Name", fName functionI)
                              , ("Module", fModule functionI)] 
                              ++ (signature $ (\((modName,_), tExpr) -> typeSignature modName tExpr) 
                                  $ fSignature functionI)
                              ++ (description $ fDescription functionI)      
    
-- | Generates the context information for a type
contextsT :: TypeInfo -> DocId -> [(String, String, Occurrences)]
contextsT typeI i = 
    map (addOcc  (occ i 1)) $ [("Name", tName typeI)
                              , ("Module", tModule typeI)] ++
                              (signature $ (concatMap 
                              (\((modName,fctName), tExprList) -> consSignature modName tExprList)
                                $ tSignature typeI)) ++
                              (signature $ map (\((_, fctName), _) -> fctName) $ tSignature typeI)
                              ++ (description $ tDescription typeI)       

occ :: DocId -> Word32 -> Occurrences
occ dId i = singletonOccurrence dId i

addOcc :: Occurrences -> (a,b) -> (a,b,Occurrences)
addOcc occurrence (a,b) = (a,b,occurrence)

description :: String -> [(String,String)]
description s = map (addContext "Description") $ filter (not . biasedWord) $ splitOnWhitespace s

signature :: [String] -> [(String,String)]
signature s = (addContext "Signature" (intercalate "->" s)) : map (addContext "Signature") s

addContext :: String -> String -> (String, String)
addContext context s = (context, s)

author :: String -> [(String, String)]
author a = map (addContext "Author") $ splitOnWhitespace $ a