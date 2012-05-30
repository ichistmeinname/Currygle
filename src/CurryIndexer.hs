module Main (main) 
where 
-- import Holumbus.Index.Common.LoadStore
-- import Holumbus.Index.Inverted.CompressedPrefixMem hiding (Inverted (..))
-- import qualified Holumbus.Index.Inverted.CompressedPrefixMem as PM
import Holumbus.Index.Common
-- import Holumbus.Index.Common.DocId
-- import Holumbus.Index.Common.DocIdMap
-- import Holumbus.Index.Common.Occurenceshide Inverted (..))
-- import Holumbus.Index.Common.Document
-- import Holumbus.Index.CompactIndex
-- import qualified Holumbus.Index.CompactIndex as CI
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
doc curryI = CD.singleton (Document {title = mName (moduleInfo curryI) , uri = "file:///Users/ichistmeinname/Dropbox/Uni/6/Bachelorarbeit/2012-sad-ba/CurrySearch/index/DOC_firstprog/" ++ map toLower (mName (moduleInfo curryI)) ++ ".html", custom = Just curryI})


idx :: CurryInfo -> Inverted
idx curryI = fromList emptyInverted (contextList curryI)

contextList :: CurryInfo -> [(String, String, Occurrences)]
contextList curryI = contextsMod (moduleInfo curryI) ++ concat (map contextsF (functionInfos curryI)) ++ concat (map contextsT (typeInfos curryI))

contextsMod :: ModuleInfo -> [(String, String, Occurrences)]
contextsMod moduleI = 
    map (addOcc  (occ nullDocId 1)) [("Name", mName moduleI), ("Author", mAuthor moduleI), ("Description", mDescription moduleI)]    

contextsF :: FunctionInfo -> [(String, String, Occurrences)]
contextsF functionI =
    map (addOcc  (occ nullDocId 1)) [("Name", fName functionI), ("Signature", intercalate "->" $ fSignature functionI),
        ("Description", fDescription functionI)]        
    
contextsT :: TypeInfo -> [(String, String, Occurrences)]
contextsT typeI = 
    map (addOcc  (occ nullDocId 1)) [("Name", tName typeI), ("Signature", intercalate "->" $ concat $ tSignature typeI), 
        ("Description", tDescription typeI)]        


occ :: DocId -> Word32 -> Occurrences
occ dId i = singletonOccurrence (incrDocId dId) i

addOcc :: Occurrences -> (a,b) -> (a,b,Occurrences)
addOcc occurrence (a,b) = (a,b,occurrence)

splitWhitespace :: String -> [T.Text]
splitWhitespace text = (T.splitOn  (T.pack " ") (T.pack text))

-- docsInfo2 :: Documents CurryInfo
-- docsInfo2 = snd $ insertDoc docsInfo docInfo2

-- docInfo2 :: Document CurryInfo
-- docInfo2 = Document {title = "AnotherTest", uri = "1338URI", custom = Just curryInfo}

-- idx :: Inverted
-- idx = fromList emptyInverted [("headline", "1337", occ),("headline", "Test", occ1),("headline", "AbraKadabra", occ2)]

-- occ, occ1:: Occurrences
-- occ = singletonOccurrence firstDocId 1
-- occ1 = singletonOccurrence (incrDocId firstDocId) 1

-- curryInfo2 :: CurryInfo
-- curryInfo2 = CurryInfo (ModuleInfo "FirstProg" "0.1" "S. Dylus" ["Prelude","Char","List"] "This is another test\n\n") [] []
-- curryInfo :: CurryInfo
-- curryInfo = CurryInfo (ModuleInfo "FirstProg" "0.1" "Sandra Dylus" ["Prelude","Char","List"] "This is a test\n\n") [(FunctionInfo "nine" ["Int"] "FirstProg" [] False UnknownFR),(FunctionInfo "square" ["Int","Int"] "FirstProg" [] False UnknownFR),(FunctionInfo "two" ["Int"] "FirstProg" [] False UnknownFR),(FunctionInfo "three" ["Int"] "FirstProg" [] False UnknownFR),(FunctionInfo "test" ["Int","[Char]","Float","[Char]"] "FirstProg" [] False UnknownFR),(FunctionInfo "listOfNumber" ["Int","[Int]"] "FirstProg" "comment for listOfNumbers" False UnknownFR),(FunctionInfo "aTupel" ["[Char]","[Char]","([Char],[Char],[Char])"] "FirstProg" [] False UnknownFR)] [(TypeInfo "AnotherInt" [["(Int,Int)"]] "FirstProg" []),(TypeInfo "Tree" [["EmptyTree"],["Node","Tree","[Int]","Tree"]] "FirstProg" [])]