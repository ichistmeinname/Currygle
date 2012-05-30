
module Main (main) 

where 

import Holumbus.Index.Common
import Holumbus.Crawler.IndexerCore (IndexerState (..))
import qualified Holumbus.Index.CompactDocuments as CD
import Data.Word (Word32)
import Data.Char (toLower)
import Data.Text (splitOn, pack, unpack)
import qualified Data.Text as T
import Data.List
import CurryInfo
import IndexTypes


main :: IO ()
main = do
    putStr $ "Writing index ..."
    curryDoc <- loadFromCurryFile $ filePath ++ "firstprog.cdoc"
    let curryState = IndexerState (idx curryDoc) (doc curryDoc)
    putStr $ " done!\n"
    writeSearchBin "../index/ix.bin" $ curryState

-- curryState :: Inverted -> Documents CurryInfo -> CurryIndexerState
-- curryState index documents = IndexerState index documents

doc :: CurryInfo ->  Documents CurryInfo
doc curryI = CD.singleton (Document {title = mName (moduleInfo curryI) , uri = {-map toLower (mName $ moduleInfo curryI) ++ ".html"-} "http://www.heise.de", custom = Just curryI})

idx :: CurryInfo -> Inverted
idx curryI = fromList emptyInverted (contextList curryI)

contextList :: CurryInfo -> [(String, String, Occurrences)]
contextList curryI = contextsMod (moduleInfo curryI) ++ concat (map contextsF (functionInfos curryI)) ++ concat (map contextsT (typeInfos curryI))

contextsMod :: ModuleInfo -> [(String, String, Occurrences)]
contextsMod moduleI = 
    map (addOcc  (occ nullDocId 1)) $ [("Name", mName moduleI)] 
                                   ++ (author $ mAuthor moduleI) 
                                   ++ (description $ mDescription moduleI)

contextsF :: FunctionInfo -> [(String, String, Occurrences)]
contextsF functionI =
    map (addOcc  (occ nullDocId 1)) $ [("Name", fName functionI)
                                    , ("Signature", listToSignature $ fSignature functionI)] 
                                   ++ (description $ fDescription functionI)      
    
contextsT :: TypeInfo -> [(String, String, Occurrences)]
contextsT typeI = 
    map (addOcc  (occ nullDocId 1)) $ [("Name", tName typeI)
                                    , ("Signature", listToSignature $ concat $ tSignature typeI)]
                                   ++ (description $ tDescription typeI)       


occ :: DocId -> Word32 -> Occurrences
occ dId i = singletonOccurrence (incrDocId dId) i

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