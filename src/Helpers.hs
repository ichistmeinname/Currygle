{- |
Module      :  Helpers
Description :  
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de(TypeExpr (..), QName)

Stability   :  experimental
Portability :  portable

A module full of helper functions.
-}

module Helpers
where

import qualified Data.Text      as T
import           Data.List
import           Data.Char

import           CurryInfo

import qualified Text.XmlHtml   as X

-- when types are used in the module where they're defined, a qualified name is not necessary,
-- same holds for types of the prelude
isQualifiedName :: String -> String -> Bool
isQualifiedName moduleName fModuleName = moduleName == fModuleName || fModuleName == "Prelude"

qualifiedName :: String -> String -> String -> String
qualifiedName moduleName fModuleName funcName = 
  if isQualifiedName moduleName fModuleName then funcName else moduleName ++ "." ++ funcName

-- parens a given string when parens flag is true
paren :: Bool -> String -> String
paren parens str 
    | parens    = "(" ++ str ++ ")"
    | otherwise = str 

-- splits strings at whitespaces (ex.: for better handling of comments)
splitOnWhitespace :: String -> [String]
splitOnWhitespace = splitStringOn " "

splitOnArrow :: String -> [String]
splitOnArrow = splitStringOn "->"

splitStringOn :: String -> String -> [String]
splitStringOn splitter text = map T.unpack (T.splitOn  (T.pack splitter) (T.pack text))

-- returns a path that ends with '/'
fullPath :: String -> String
fullPath path  = if last path == '/' then path else path ++ "/"

-- Words shorter than 2 characters are biased
biasedWord :: String -> Bool
biasedWord s = length s < 3

-------------------------------------
--- Some pretty printig functions ---
-------------------------------------

-- pretty print for special types like lists or tupels
prettyPrintSpecialType :: String -> String -> [TypeExpr] -> String
prettyPrintSpecialType _ name [] = name
prettyPrintSpecialType name funcName [tExpr] 
    | funcName == "[]" = "[" ++  (prettyPrint name tExpr) ++ "]"
    | otherwise = funcName ++ " " ++ (prettyPrint name tExpr)
prettyPrintSpecialType name funcName tExprList@(_:_:_)  
    | head funcName == '(' =
    "(" ++
    intercalate "," (map (prettyPrint name) tExprList)
    ++ ")"
    | otherwise = funcName ++ " " ++ concat (map (prettyPrint name) tExprList)

prettyPrint :: String -> TypeExpr -> String
prettyPrint modName (TCons (mName2, fName2) []) = 
  qualifiedName modName mName2 fName2
prettyPrint modName (TCons (mName2, fName2) tExprList) =
    let name = qualifiedName modName mName2 fName2
    in prettyPrintSpecialType modName name tExprList
prettyPrint _ (TVar i) = [chr (i+97)]
prettyPrint modName (FuncType tExpr1 tExpr2) =
    prettyPrint modName tExpr1 ++ "->" ++ prettyPrint modName tExpr2
prettyPrint _ Undefined = ""

signatureList :: String -> TypeExpr -> [String]
signatureList modName (TCons (mName2, fName2) tExprList) = 
  [prettyPrintSpecialType modName name tExprList]
 where name = qualifiedName modName mName2 fName2
signatureList modName (FuncType tExpr1 tExpr2) = 
  case tExpr1 of
    FuncType _ _ -> [paren True (prettyPrint modName tExpr1)] ++ signatureList modName tExpr2
    _            -> signatureList modName tExpr1 ++ signatureList modName tExpr2
signatureList _ (TVar i) = [[chr (i+97)]]
signatureList _ Undefined = []

consSignature :: String -> [TypeExpr] -> [String]
consSignature modName = concatMap (signatureList modName)

-- Converts a TypeInfo signature (QName, [TypeExpr) to a tuple of list of strings and a string,
-- i.e. (["a","Maybe a"], "Just")
consToList :: String -> (QName, [TypeExpr]) -> ([String], String)
consToList typeName ((modName, consName), tExprList) = 
   ((consSignature modName tExprList) ++ [typeName], consName)

-- Converts a FunctionInfo signature (QName, TypeExpr) to a list of strings,
-- i.e. ["Int","(Int->[Char])","Bool"]
typeToList :: (QName, TypeExpr) -> [String]
typeToList ((modName, _), tExpr) =
  signatureList modName tExpr 

-- Pretty printing for signatures 
listToSignature :: [String] -> String
listToSignature = intercalate "->"


-- Converts a String to an Int, returns defaultValue if conversion fails.
strToInt :: Int -> String -> Int
strToInt defaultValue str
  | length readsResult > 0 = fst $ head readsResult
  | otherwise = defaultValue
  where
  readsResult = reads str

-- | creates a HTML Txt Node
htmlTextNode :: String -> X.Node
htmlTextNode text = X.TextNode $ T.pack text

makeHtml :: String -> String -> String -> [X.Node] -> X.Node
makeHtml attribute link css =
  X.Element (T.pack attribute) (href ++ cssClass)
 where cssClass = eitherOr css [] [(T.pack "class", T.pack css)]
       href     = eitherOr link [] [(T.pack "href", T.pack link)]
       eitherOr str nothing result = if str == "" then nothing else result

htmlDiv :: String -> [X.Node] -> X.Node
htmlDiv = makeHtml "div" "" 

htmlUl :: [X.Node] -> X.Node
htmlUl = makeHtml "ul" "" ""

htmlLiClass :: String -> [X.Node] -> X.Node
htmlLiClass = makeHtml "li" ""

htmlLi :: [X.Node] -> X.Node
htmlLi = htmlLiClass ""

htmlHref :: String -> String -> [X.Node] -> X.Node
htmlHref = makeHtml "a"

htmlh3 :: [X.Node] -> X.Node
htmlh3 = makeHtml "h3" "" ""

htmlP :: [X.Node] -> X.Node
htmlP = makeHtml "p" "" ""

icon :: [X.Node] -> X.Node
icon = makeHtml "i" "" "icon-leaf"

moduleText :: String -> (String, String)
moduleText text = ("module", text)

makeResult :: String -> String -> (String, String) -> String -> [(String, String)] -> X.Node
makeResult title uri (oAtt, oText) descr sig =
  resultItem uri (captionText title ++ 
                 [htmlUl (sigHtml signature ++ othersHtml ++ descriptionHtml descr)])
 where signature = ifNotEmpty sig [htmlTypeCons $ map (\(name, consSig) -> name ++ " :: " ++ consSig) sig]
       othersHtml = ifNotEmpty oText [htmlLiClass oAtt [htmlTextNode $ modOrAuthorText oAtt ++ oText]]

modOrAuthorText :: String -> String
modOrAuthorText othersAttribute
  | othersAttribute == "module" = "defined in "
  | othersAttribute == "author" = "coded by "
  | otherwise                   = ""

captionText :: String -> [X.Node]
captionText text = [htmlh3 ([icon []] ++ [htmlTextNode text])]

sigHtml :: [X.Node] -> [X.Node]
sigHtml content = [htmlLiClass "signature" content]

ifNotEmpty :: Eq a => [a] -> [b] -> [b]
ifNotEmpty list content = if list == [] then [] else content

resultItem :: String -> [X.Node] -> X.Node
resultItem uri result = htmlDiv "" [htmlHref uri "result-item" result]

htmlTypeCons :: [String] -> X.Node
htmlTypeCons constr = htmlTextNode ("Constructors: " ++ intercalate ", " constr)

consPairs :: [String] -> [String] -> [(String, String)]
consPairs (e1:l1) (e2:l2) = (e1, e2) : consPairs l1 l2
consPairs _ _             = []   

filterOptionalList :: String -> [(String, a)] -> [(String,a)]
filterOptionalList title = filter (\(itemTitle,_) -> itemTitle == title)                 

descriptionHtml :: String -> [X.Node]
descriptionHtml text = [htmlLiClass "description" [htmlTextNode descr]]
 where descr = if (length $ words text) > numTeaserWords then teaserText ++ " ..."
                  else teaserText
       teaserText = unwords $ take numTeaserWords $ words text

-- | Number of words contained in the teaser of the description text.
numTeaserWords :: Int
numTeaserWords = 30
                      
makeTitle :: String -> String
makeTitle [] = []
makeTitle (t:ext) = toUpper t : ext ++ ": "

-- | Creates the pagination as htmlLink (X.Node) used in the Pager Splice.
mkPagerLink :: String -> Int -> Int -> [X.Node]
mkPagerLink query actPage number =
  [htmlDiv "pagination pagination-centered" 
    [htmlUl (pagination query actPage number)]]

pagination :: String -> Int -> Int -> [X.Node]
pagination query actPage numberOfPages = 
  previous ++ firstPage ++ map (pageLinks query actPage) pageNumbers ++ lastPage ++ next
 where previous = if actPage > 1 then page _previousPage "" 
                                 else page _previousPage "disabled"
       next     = if actPage < numberOfPages then page _nextPage "" 
                                             else page _nextPage "disabled"
       pageNumber name
         | name ==  _previousPage = actPage - 1 
         | name == _nextPage      = actPage + 1
         | otherwise              = strToInt 0 name
       firstPage = -- add first page if it's out of range
         if head pageNumbers < 2 then []
            else page "1" "" ++ page "..." "disabled"
       lastPage  = -- add last page if it's out of range
         if last pageNumbers > (numberOfPages-1)  then []
            else page "..." "disabled" ++ page (show numberOfPages) ""
       link name = paginationLink query (pageNumber name)
       pageNumbers -- just show a range of 9 pages
         | numberOfPages < 10 = [1..numberOfPages]
         | actPage + 4 >= numberOfPages = [start..numberOfPages]
         | actPage < 5 = [1..9]
         | otherwise   = [(max (actPage-4) 1)..(min (actPage+4) numberOfPages)]
       start = 2 * numberOfPages - actPage - 10
       page name cssClass = 
         [htmlLiClass cssClass [htmlHref (link name) "" [htmlTextNode name]]]

-- Creates a htmt list item (possibly with a class attribute) with a html link
-- <li class="active"><a href=$link>$pageNumber</a></li>
pageLinks :: String -> Int -> Int -> X.Node
pageLinks query actPage pageNumber =
  htmlLiClass cssStyle [htmlHref (paginationLink query pageNumber) "" [htmlTextNode $ show pageNumber]]
 where cssStyle = if actPage == pageNumber then "active" else ""
       
-- Concats the link for the pagination for a given query and page
paginationLink :: String -> Int -> String
paginationLink query nr = "/querypage?query=" ++ query ++ "&page=" ++ show nr

_previousPage :: String 
_previousPage = "Prev"

_nextPage :: String
_nextPage = "Next"

-- ------------------------------------------------------------------------------
-- | The help-text rendered when no results are found

-- examples :: X.Node
-- examples
--     = X.Element (T.pack "div")
--       [(T.pack "class", T.pack "examples")]
--       [ htmlParaTextNode "Beispiele für Suchanfragen:"
--       , htmlList "examples"
--         [ htmlListItem "" $
--           X.Element (T.pack "div")
--           [ (T.pack "class", T.pack "example") ]
--           [ htmlLink "" "querypage?query=Holumbus%20AND%20Hayoo&button=Suchen" "Holumbus AND Hayoo"
--           , htmlSpanTextNode "" " sucht nach Seiten, die sowohl das Wort "
--           , htmlSpanTextNode "green" "Holumbus"
--           , htmlSpanTextNode "" " als auch das Wort "
--           , htmlSpanTextNode "green" "Hayoo"
--           , htmlSpanTextNode "" " enthalten. (Kurzform: "
--           , htmlLink "" "querypage?query=Holumbus%20Hayoo&button=Suchen" "Holumbus Hayoo"
--           , htmlSpanTextNode "" ")."
--           ]
--         , htmlListItem "" $
--           X.Element (T.pack "div")
--           [ (T.pack "class", T.pack "example") ]
--           [ htmlLink "" "querypage?query=Holumbus%20OR%20Hayoo&button=Suchen" "Holumbus OR Hayoo"
--           , htmlSpanTextNode "" " sucht nach Seiten, die mindestens eines der beider Wörter enthalten."
--           ]
--         , htmlListItem "" $
--           X.Element (T.pack "div")
--           [ (T.pack "class", T.pack "example") ]
--           [ htmlLink "" "querypage?query=Haskell%20AND%20NOT%20(Holumbus%20OR%20Hayoo)&button=Suchen" "Haskell AND NOT (Holumbus OR Hayoo)"
--           , htmlSpanTextNode "" " sucht nach Seiten, die das Wort "
--           , htmlSpanTextNode "green" "Haskell"
--           , htmlSpanTextNode "" ", aber keines der Wörter "
--           , htmlSpanTextNode "green" "Holumbus"
--           , htmlSpanTextNode "" " oder "
--           , htmlSpanTextNode "green" "Hayoo"
--           , htmlSpanTextNode "" " enthalten."
--           ]
--         , htmlListItem "" $
--           X.Element (T.pack "div")
--           [ (T.pack "class", T.pack "example") ]
--           [ htmlLink "" "querypage?query=Seminar%20Juni%202011&button=Suchen" "Seminar Juni 2011"
--           , htmlSpanTextNode "" " sucht nach allen Vorkommen des Textes "
--           , htmlSpanTextNode "green" "Seminar"
--           , htmlSpanTextNode "" " und einem Datum des Monats Juni im Jahr 2011. Gefunden werden Seiten, die Daten wie "
--           , htmlSpanTextNode "green" "1.5.'11"
--           , htmlSpanTextNode "" " oder "
--           , htmlSpanTextNode "green" "5. Mai"
--           , htmlSpanTextNode "" " (bei Daten ohne Jahresangabe wird das aktuelle Jahr angenommen) enthalten. Genauso funktionieren viele weitere gängige Datumsformate wie z.B.: "
--           , htmlLink "" "querypage?query=21.%20September%20um%2013%20Uhr&button=Suchen" "21. September um 13 Uhr"
--           , htmlSpanTextNode "" ", "
--           , htmlLink "" "querypage?query=3.6.%2011:00&button=Suchen" "13.6. 9:00"
--           ]
--         , htmlListItem "" $
--           X.Element (T.pack "div")
--           [ (T.pack "class", T.pack "example") ]
--           [ htmlLink "" "querypage?query=PTL%20dieser%20Monat&button=Suchen" "PTL dieser Monat"
--           , htmlSpanTextNode "" " sucht nach allen Vorkommen des Textes "
--           , htmlSpanTextNode "green" "PTL"
--           , htmlSpanTextNode "" " und einem Datum des aktuellen Monats, sowie nach dem Text "
--           , htmlSpanTextNode "green" "dieser Monat"
--           , htmlSpanTextNode "" ". Genauso funktionieren folgende weitere Abkürzungen zum Suchen nach Daten: "
--           , htmlLink "" "querypage?query=heute&button=Suchen" "heute"
--           , htmlSpanTextNode "" ", "
--           , htmlLink "" "querypage?query=diese%20Woche&button=Suchen" "diese Woche"
--           , htmlSpanTextNode "" ", "
--           , htmlLink "" "querypage?query=nächste%20Woche&button=Suchen" "nächste Woche"
--           , htmlSpanTextNode "" ", "
--           , htmlLink "" "querypage?query=dieser%20Monat&button=Suchen" "dieser Monat"
--           , htmlSpanTextNode "" ", "
--           , htmlLink "" "querypage?query=nächster%20Monat&button=Suchen" "nächster Monat"
--           {-
--           , htmlSpanTextNode "" ", "
--           , htmlLink "" "querypage?query=übernächster%20Monat&button=Suchen" "übernächster Monat"
--           , htmlSpanTextNode "" ", "
--           , htmlLink "" "querypage?query=dieses%20Jahr&button=Suchen" "dieses Jahr"
--           , htmlSpanTextNode "" ", "
--           , htmlLink "" "querypage?query=nächstes%20Jahr&button=Suchen" "nächstes Jahr"
--           -- -}
--           , htmlSpanTextNode "" "."
--           ]
--         , htmlListItem "" $
--           X.Element (T.pack "div")
--           [ (T.pack "class", T.pack "example") ]
--           [ htmlSpanTextNode "" "Die Eingaben der Terminsuche können abgekürzt werden: "
--           , htmlLink "" "querypage?query=di%20wo&button=Suchen" "di wo"
--           , htmlSpanTextNode "" " oder "
--           , htmlLink "" "querypage?query=diwo&button=Suchen" "diwo"
--           , htmlSpanTextNode "" " werden wie "
--           , htmlLink "" "querypage?query=diese%20Woche&button=Suchen" "diese Woche"
--           , htmlSpanTextNode "" " interpretiert."
--           ]
--         , htmlListItem "" $
--           X.Element (T.pack "div")
--           [ (T.pack "class", T.pack "example") ]
--           [ htmlSpanTextNode "" "Termine für bestimmte Monate können ebenfalls abgekürzt gesucht werden. Bei einer Eingabe von "
--           , htmlLink "" "querypage?query=September&button=Suchen" "September"
--           , htmlSpanTextNode "" " oder "
--           , htmlLink "" "querypage?query=sep&button=Suchen" "sep"
--           , htmlSpanTextNode "" " werden Termine im kommenden (oder laufenden) September gesucht."
--           ]
--         , htmlListItem "" $
--           X.Element (T.pack "div")
--           [ (T.pack "class", T.pack "example") ]
--           [ htmlLink "" "querypage?query=Kalender%20dieser Monat&button=Suchen" "Kalender dieser Monat"
--           , htmlSpanTextNode "" " findet unter anderem die gesuchten Daten im FH Wedel Kalender. Die dort aufgelisteten Daten k\246nnen direkt angeklickt werden um zu den entsprechenden Unterseiten zu gelangen."
--           ]
--         , htmlListItem "" $
--           X.Element (T.pack "div")
--           [ (T.pack "class", T.pack "example") ]
--           [ htmlSpanTextNode "" "Die Suche kann auf bestimmte URLs eingeschränkt werden: "
--           , htmlLink "" "querypage?query=href:vorlesungen/java%20AND%20Stack&button=Suchen" "href:vorlesungen/java AND Stack"
--           , htmlSpanTextNode "" " sucht in allen Dokumenten, deren URL den Teilpfad "
--           , htmlSpanTextNode "green" "vorlesungen/java"
--           , htmlSpanTextNode ""      " enthält, nach dem Wort "
--           , htmlSpanTextNode "green" "Stack"
--           , htmlSpanTextNode ""      ". Hiermit kann die Suche zum Beispiel auf Unterlagen zu einer bestimmten Vorlesung eingeschr\228nkt werden."
--           ]
--         ]
--       ]
