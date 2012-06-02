
module Main (main) 

where 

import Holumbus.Index.Common
import Holumbus.Crawler.IndexerCore (IndexerState (..))
import qualified Holumbus.Index.CompactDocuments as CD
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
    let curryModState = IndexerState (idx (contextsMod) $ moduleInfo curryDoc) (docM $ moduleInfo curryDoc)
    let curryFctState = unionIxDoc unionDoc $ zipWith (\i d -> IndexerState i d) (map (idx contextsF) $ functionInfos curryDoc) (map docF $ functionInfos curryDoc)
    let curryTypeState = unionIxDoc unionDoc $ zipWith (\i d -> IndexerState i d) (map (idx contextsT) $ typeInfos curryDoc) (map docT $ typeInfos curryDoc)
    putStr $ " done!\n"
    writeSearchBin "../index/ix-mod.bin" $ curryModState
    writeSearchBin "../index/ix-fct.bin" $ curryFctState
    writeSearchBin "../index/ix-type.bin" $ curryTypeState



-- unionIxDoc :: [CurryFctIndexerState]  -> (IndexerState i doc, DocId)
-- -- unionIxDoc (IndexerState i1 d1) (IndexerState i2 d2) = unionDocIndex i1 d1 i2 d2
-- -- unionIxDoc is1 is2 = unionDocIndex is1 is2
-- unionIxDoc [] = (\(IndexerState i (Documents d)) -> (i, d)) emptyCurryFctState
-- -- unionIxDoc (IndexerState is1 doc1 : iss) = unionDocIndex (IndexerState is1 doc1) (unionIxDoc iss)

-- DocId . (1+) . theDocId
unionDoc :: (Binary a) => [IndexerState Inverted Documents a] -> Documents a
unionDoc iss = let docs = map (\(IndexerState _ doc1) -> doc1) iss
              in foldr unionDocs CD.emptyDocuments (editIds docs)
                -- where editIds :: [Documents FunctionInfo] -> [Documents FunctionInfo]
                      -- editIds [df1] = df1
                      -- editIds (df1 : df2 : dfs) =  (editDocIds (addDocId (CD.lastDocId $ df1))) df2) : (editIds dfs)

editIds :: (Binary a) => [Documents a] -> [Documents a]
editIds [] = []
editIds [info] = [editDocIds incrDocId info]
editIds (df1 : df2 : dfs) = let newDf = editDocIds (addDocId (CD.lastDocId $ df1)) df2
                            in newDf : editIds (newDf : dfs)

-- unionDT :: (Binary a) => [IndexerState Inverted Documents a] -> Documents a
-- unionDT iss = let docs = map (\(IndexerState _ doc1) -> doc1) iss
--               in foldl unionDocs CD.emptyDocuments docs

unionIxDoc :: ([IndexerState Inverted Documents a] -> (Documents a)) -> [IndexerState Inverted Documents a] -> IndexerState Inverted Documents a
unionIxDoc unionD iss = IndexerState (unionI iss) (unionD iss)

unionI :: [IndexerState Inverted Documents a] -> Inverted
unionI iss = let ixs = map (\(IndexerState i _) -> i) iss
             in foldl mergeIndexes emptyInverted ixs

-- curryState :: Inverted -> Documents CurryInfo -> CurryIndexerState
-- curryState index documents = IndexerState index documents

-- doc :: CurryInfo ->  Documents CurryInfo
-- doc curryI = CD.singleton (Document {title = mName (moduleInfo curryI) , uri = {-map toLower (mName $ moduleInfo curryI) ++ ".html"-} "http://www.heise.de", custom = Just curryI})

docM :: ModuleInfo -> Documents ModuleInfo
docM modI = CD.singleton (Document {title = mName modI, uri = mName modI ++ ".html", custom = Just modI})

docF :: FunctionInfo -> Documents FunctionInfo
docF fctI = CD.singleton (Document {title = fName fctI, uri = fModule fctI ++ "/" ++ fName fctI ++ ".html", custom = Just fctI})

docT :: TypeInfo -> Documents TypeInfo
docT typeI = CD.singleton (Document {title = tName typeI, uri = tModule typeI ++ "/" ++ tName typeI ++ ".html", custom = Just typeI})

-- idx :: CurryInfo -> Inverted
-- idx curryI = fromList emptyInverted (contextList curryI)

-- doc :: a -> 

idx :: (a -> [(String, String, Occurrences)]) -> a -> Inverted
idx contextList info = fromList emptyInverted $ contextList info

-- contextList :: CurryInfo -> [(String, String, Occurrences)]
-- contextList curryI = contextsMod (moduleInfo curryI) ++ concat (map contextsF (functionInfos curryI)) ++ concat (map contextsT (typeInfos curryI))

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