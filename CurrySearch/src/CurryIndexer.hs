
module Main (main) 

where 

import Holumbus.Index.Common
import Holumbus.Crawler.IndexerCore (IndexerState (..))
-- import Data.Word (Word32)
-- import Data.Char (toLower)
-- import Data.Text (splitOn, pack, unpack)
import qualified Data.Text as T (splitOn, pack, unpack)
import Data.List
import Data.Binary
import CurryInfo
import IndexTypes


main :: IO ()
main = do
    putStr $ "Writing index ..."
    curryDoc <- loadFromCurryFile $ filePath ++ "firstprog.cdoc"
    let curryModState = ixDoc contextsMod [moduleInfo curryDoc] [doc mName mName (moduleInfo curryDoc)] emptyCurryModState
    let curryFctState = ixDoc contextsF (functionInfos curryDoc) (map (doc fName fName) (functionInfos curryDoc)) emptyCurryFctState
    let curryTypeState = ixDoc contextsT (typeInfos curryDoc) (map (doc tName tName) (typeInfos curryDoc)) emptyCurryTypeState
    putStr $ " done!\n"
    writeSearchBin "../index/ix-mod.bin" $ curryModState
    writeSearchBin "../index/ix-fct.bin" $ curryFctState
    writeSearchBin "../index/ix-type.bin" $ curryTypeState
    -- return ()

ixDoc :: (Binary a) => (a -> DocId -> [(String, String, Occurrences)]) -> [a] -> [Document a] -> IndexerState Inverted Documents a -> IndexerState Inverted Documents a
ixDoc contextList (info:infos) (doc1:docs) (IndexerState ix dc) = 
    let (docId, docs') = insertDoc dc doc1
        idx'           = mergeIndexes ix $ idx contextList info docId
    in ixDoc contextList infos docs (IndexerState idx' docs')
ixDoc _ _ _ is = is

doc :: (Binary a) => (a -> String) -> (a -> String) -> a -> Document a
doc fiName fiUri info = Document {title = fiName info, uri = fiUri info ++ ".html", custom = Just info}

idx :: (a -> DocId -> [(String, String, Occurrences)]) -> a -> DocId -> Inverted
idx contextList info i = fromList emptyInverted $ contextList info i

contextsMod :: ModuleInfo -> DocId -> [(String, String, Occurrences)]
contextsMod moduleI i = 
    map (addOcc  (occ i 1)) $ [("Name", mName moduleI)] 
                                   ++ (author $ mAuthor moduleI) 
                                   ++ (description $ mDescription moduleI)

contextsF :: FunctionInfo -> DocId -> [(String, String, Occurrences)]
contextsF functionI i =
    map (addOcc  (occ i 2)) $ [("Name", fName functionI)
                                    , ("Signature", listToSignature $ fSignature functionI)] 
                                   ++ (description $ fDescription functionI)      
    
contextsT :: TypeInfo -> DocId -> [(String, String, Occurrences)]
contextsT typeI i = 
    map (addOcc  (occ i 1)) $ [("Name", tName typeI)
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

splitOnWhitespace :: String -> [String]
splitOnWhitespace text = map T.unpack (T.splitOn  (T.pack " ") (T.pack text))

listToSignature :: [String] -> String
listToSignature xs = intercalate "->" xs

biasedWord :: String -> Bool
biasedWord s = length s < 3