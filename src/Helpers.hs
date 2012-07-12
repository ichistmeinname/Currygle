{- |
Module      :  Helpers
Description :  
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

A module full of helper functions.
-}

module Helpers
where

import qualified Data.Text      as T
import           Data.List
import           Data.Char


import qualified Text.XmlHtml   as X

-- ----------------------------------------------------------------------------

type QName = (String, String)

data TypeExpr =
     TVar Int              
   | FuncType TypeExpr TypeExpr     
   | TCons QName [TypeExpr]  
   | Undefined   
   deriving (Read, Show)   

emptyQName :: QName
emptyQName = ("","")

-- pretty print for special types like lists or tupels
prettyPrintSpecialType :: String -> String -> [TypeExpr] -> String
prettyPrintSpecialType _ _ [] = []	      -- shouldn't occur
prettyPrintSpecialType name fName [tExpr] 
    | fName == "[]" = "[" ++  concat (typeSignature name tExpr) ++ "]"
    | otherwise = fName ++ " " ++ concat (typeSignature name tExpr)
prettyPrintSpecialType name fName tExprList@(_:_:_)  
    | head fName == '(' =
    "(" ++ 
    intercalate "," (concatMap (typeSignature name) tExprList)
    ++ ")"
    | otherwise = fName ++ " " ++ concat (concatMap (typeSignature name) tExprList)

prettyPrint :: String -> TypeExpr -> String
prettyPrint modName (TCons (mName2, fName2) []) = 
    let name = if qualifiedName modName mName2 then fName2 else modName++"."++fName2
    in name
prettyPrint modName (TCons (mName2, fName2) tExprList) =
    let name | qualifiedName modName mName2  = fName2
             | otherwise = modName++"."++fName2
    in prettyPrintSpecialType modName name tExprList
prettyPrint _ (TVar i) = [chr (i+97)]
prettyPrint modName (FuncType tExpr1 tExpr2) =
    prettyPrint modName tExpr1 ++ "->" ++ prettyPrint modName tExpr2
prettyPrint _ Undefined = ""

-- generate type signature  
typeSignature :: String -> TypeExpr -> [String]
typeSignature modName (TCons (mName2, fName2) []) = 
    let name = if qualifiedName modName mName2 then fName2 else modName++"."++fName2
    in [name]
typeSignature modName (TCons (mName2, fName2) tExprList) =
    let name | qualifiedName modName mName2 = fName2
             | otherwise = modName++"."++fName2
    in [prettyPrintSpecialType modName name tExprList]
typeSignature modName (FuncType tExpr1@(FuncType _ _) tExpr2) =
    paren True (prettyPrint modName tExpr1) : typeSignature modName tExpr2
typeSignature modName tExpr@(FuncType _ _) = 
    [prettyPrint modName tExpr]
typeSignature _ (TVar i) = [[chr (i+97)]]
typeSignature _ Undefined = []

consSignature :: String -> [TypeExpr] -> [String]
consSignature modName = concatMap (typeSignature modName)

consToList :: (QName, [TypeExpr]) -> [String]
consToList ((modName, fctName), tExprList) = 
    [fctName] ++ [" "] ++ consSignature modName tExprList

qualifiedName :: String -> String -> Bool
qualifiedName name1 name2 = name1 == name2 || name2 == "Prelude"

paren :: Bool -> String -> String
paren parens str 
    | parens    = "(" ++ str ++ ")"
    | otherwise = str 

--  Pretty printing for signatures 
listToSignature :: [String] -> String
listToSignature = intercalate "->"

-- splits strings at whitespaces (ex.: for better handling of comments)
splitOnWhitespace :: String -> [String]
splitOnWhitespace text = map T.unpack (T.splitOn  (T.pack " ") (T.pack text))

-- Words shorter than 2 characters are biased
biasedWord :: String -> Bool
biasedWord s = length s < 3

-- ------------------------------------------------------------
--
-- text preprocessing
--
-- ------------------------------------------------------------

deleteNotAllowedChars           :: String -> String
deleteNotAllowedChars           = map notAllowedToSpace
    where
    notAllowedToSpace c
        | isAllowedWordChar c   = c
        | otherwise             = ' '

isAllowedWordChar   :: Char -> Bool
isAllowedWordChar c = c `elem` "_-"

-- ----------------------------------------------------------------------------
-- | Make an one-item-List      

box :: a -> [a]
box x = [x]

-- ------------------------------------------------------------------------------
-- | convert a String to an Int.
-- | returns defaultValue if conversion fails

strToInt :: Int -> String -> Int
strToInt defaultValue str
  | length readsResult > 0 = fst $ head readsResult
  | otherwise = defaultValue
  where
  readsResult = reads str


saveHead :: [a] -> a -> a
saveHead [] x = x
saveHead (x:_) _   = x

-- ------------------------------------------------------------------------------
-- | creates a HTML List-Item with css-class-attribute

htmlList :: String -> [X.Node] -> X.Node
htmlList cssClass =
  X.Element (T.pack "ul")
    (
      if cssClass == ""
        then []
        else [(T.pack "class", T.pack cssClass)]
    )

-- ------------------------------------------------------------------------------
-- | creates a HTML List-Item with css-class-attribute

htmlListItem :: String -> X.Node -> X.Node
htmlListItem cssClass xNode =
  X.Element (T.pack "li")
    (
      if cssClass == ""
        then []
        else [(T.pack "class", T.pack cssClass)]
    )
    [xNode]

 -- ------------------------------------------------------------------------------
-- | creates a HTML Txt Node

htmlTextNode :: String -> X.Node
htmlTextNode text = X.TextNode $ T.pack text

-- ------------------------------------------------------------------------------
-- | creates a HTML Txt Node in a <span class="???"></span> element

htmlSpanTextNode :: String -> String -> X.Node
htmlSpanTextNode cssClass text =
  X.Element (T.pack "span")
    (
      if cssClass == ""
        then []
        else [(T.pack "class", T.pack cssClass)]
    )
    [htmlTextNode text]

-- ------------------------------------------------------------------------------
-- | creates a HTML Txt Node in a <p></p> element

htmlParaTextNode :: String -> X.Node
htmlParaTextNode text =
  X.Element (T.pack "p")
    []
    [htmlTextNode text]

-- ------------------------------------------------------------------------------
-- | creates a HTML Info Node

htmlLink :: String -> String -> String -> X.Node
htmlLink cssClass href text =
  X.Element (T.pack "a")
    (
      [(T.pack "href", T.pack href)]
      ++
      (
        if cssClass == ""
          then []
          else [(T.pack "class", T.pack cssClass)]
      )
    )
    [htmlTextNode text]

-- ------------------------------------------------------------------------------
-- | creates a HTML Info Node

htmlLink' :: String -> String -> X.Node -> X.Node
htmlLink' cssClass href xNode =
  X.Element (T.pack "a")
    (
      [(T.pack "href", T.pack href)]
      ++
      (
        if cssClass == ""
          then []
          else [(T.pack "class", T.pack cssClass)]
      )
    )
    [xNode]

-- ------------------------------------------------------------------------------
-- | creates a HTML Link used in the Pager Splice
-- |   query: the search-query
-- |   number: Number displayed in the Pager-Link (the text node of the link)
-- |   takeHits: Number of Hits to be displayed per Site
-- |   dropHits: Number of Hits to be dropped from the Result of all Document-Hits
-- | i.e. for Pager-Link No. 4, searching for "Wedel": <a href="/querypage?query=Wedel&takeHits=10&dropHits=30"> 4 </a>

mkPagerLink :: String -> Int -> Int -> X.Node
mkPagerLink query actPage number =
  htmlLink cssClass
    ("/querypage?query=" ++ query ++ "&page=" ++ pageNum)
    (" " ++ pageNum ++ " ")
  where
    pageNum = show number
    cssClass = if actPage == number
                  then "actPage"
                  else ""


-- ------------------------------------------------------------------------------
-- | The help-text rendered when no results are found

examples :: X.Node
examples
    = X.Element (T.pack "div")
      [(T.pack "class", T.pack "examples")]
      [ htmlParaTextNode "Beispiele für Suchanfragen:"
      , htmlList "examples"
        [ htmlListItem "" $
          X.Element (T.pack "div")
          [ (T.pack "class", T.pack "example") ]
          [ htmlLink "" "querypage?query=Holumbus%20AND%20Hayoo&button=Suchen" "Holumbus AND Hayoo"
          , htmlSpanTextNode "" " sucht nach Seiten, die sowohl das Wort "
          , htmlSpanTextNode "green" "Holumbus"
          , htmlSpanTextNode "" " als auch das Wort "
          , htmlSpanTextNode "green" "Hayoo"
          , htmlSpanTextNode "" " enthalten. (Kurzform: "
          , htmlLink "" "querypage?query=Holumbus%20Hayoo&button=Suchen" "Holumbus Hayoo"
          , htmlSpanTextNode "" ")."
          ]
        , htmlListItem "" $
          X.Element (T.pack "div")
          [ (T.pack "class", T.pack "example") ]
          [ htmlLink "" "querypage?query=Holumbus%20OR%20Hayoo&button=Suchen" "Holumbus OR Hayoo"
          , htmlSpanTextNode "" " sucht nach Seiten, die mindestens eines der beider Wörter enthalten."
          ]
        , htmlListItem "" $
          X.Element (T.pack "div")
          [ (T.pack "class", T.pack "example") ]
          [ htmlLink "" "querypage?query=Haskell%20AND%20NOT%20(Holumbus%20OR%20Hayoo)&button=Suchen" "Haskell AND NOT (Holumbus OR Hayoo)"
          , htmlSpanTextNode "" " sucht nach Seiten, die das Wort "
          , htmlSpanTextNode "green" "Haskell"
          , htmlSpanTextNode "" ", aber keines der Wörter "
          , htmlSpanTextNode "green" "Holumbus"
          , htmlSpanTextNode "" " oder "
          , htmlSpanTextNode "green" "Hayoo"
          , htmlSpanTextNode "" " enthalten."
          ]
        , htmlListItem "" $
          X.Element (T.pack "div")
          [ (T.pack "class", T.pack "example") ]
          [ htmlLink "" "querypage?query=Seminar%20Juni%202011&button=Suchen" "Seminar Juni 2011"
          , htmlSpanTextNode "" " sucht nach allen Vorkommen des Textes "
          , htmlSpanTextNode "green" "Seminar"
          , htmlSpanTextNode "" " und einem Datum des Monats Juni im Jahr 2011. Gefunden werden Seiten, die Daten wie "
          , htmlSpanTextNode "green" "1.5.'11"
          , htmlSpanTextNode "" " oder "
          , htmlSpanTextNode "green" "5. Mai"
          , htmlSpanTextNode "" " (bei Daten ohne Jahresangabe wird das aktuelle Jahr angenommen) enthalten. Genauso funktionieren viele weitere gängige Datumsformate wie z.B.: "
          , htmlLink "" "querypage?query=21.%20September%20um%2013%20Uhr&button=Suchen" "21. September um 13 Uhr"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=3.6.%2011:00&button=Suchen" "13.6. 9:00"
          ]
        , htmlListItem "" $
          X.Element (T.pack "div")
          [ (T.pack "class", T.pack "example") ]
          [ htmlLink "" "querypage?query=PTL%20dieser%20Monat&button=Suchen" "PTL dieser Monat"
          , htmlSpanTextNode "" " sucht nach allen Vorkommen des Textes "
          , htmlSpanTextNode "green" "PTL"
          , htmlSpanTextNode "" " und einem Datum des aktuellen Monats, sowie nach dem Text "
          , htmlSpanTextNode "green" "dieser Monat"
          , htmlSpanTextNode "" ". Genauso funktionieren folgende weitere Abkürzungen zum Suchen nach Daten: "
          , htmlLink "" "querypage?query=heute&button=Suchen" "heute"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=diese%20Woche&button=Suchen" "diese Woche"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=nächste%20Woche&button=Suchen" "nächste Woche"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=dieser%20Monat&button=Suchen" "dieser Monat"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=nächster%20Monat&button=Suchen" "nächster Monat"
          {-
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=übernächster%20Monat&button=Suchen" "übernächster Monat"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=dieses%20Jahr&button=Suchen" "dieses Jahr"
          , htmlSpanTextNode "" ", "
          , htmlLink "" "querypage?query=nächstes%20Jahr&button=Suchen" "nächstes Jahr"
          -- -}
          , htmlSpanTextNode "" "."
          ]
        , htmlListItem "" $
          X.Element (T.pack "div")
          [ (T.pack "class", T.pack "example") ]
          [ htmlSpanTextNode "" "Die Eingaben der Terminsuche können abgekürzt werden: "
          , htmlLink "" "querypage?query=di%20wo&button=Suchen" "di wo"
          , htmlSpanTextNode "" " oder "
          , htmlLink "" "querypage?query=diwo&button=Suchen" "diwo"
          , htmlSpanTextNode "" " werden wie "
          , htmlLink "" "querypage?query=diese%20Woche&button=Suchen" "diese Woche"
          , htmlSpanTextNode "" " interpretiert."
          ]
        , htmlListItem "" $
          X.Element (T.pack "div")
          [ (T.pack "class", T.pack "example") ]
          [ htmlSpanTextNode "" "Termine für bestimmte Monate können ebenfalls abgekürzt gesucht werden. Bei einer Eingabe von "
          , htmlLink "" "querypage?query=September&button=Suchen" "September"
          , htmlSpanTextNode "" " oder "
          , htmlLink "" "querypage?query=sep&button=Suchen" "sep"
          , htmlSpanTextNode "" " werden Termine im kommenden (oder laufenden) September gesucht."
          ]
        , htmlListItem "" $
          X.Element (T.pack "div")
          [ (T.pack "class", T.pack "example") ]
          [ htmlLink "" "querypage?query=Kalender%20dieser Monat&button=Suchen" "Kalender dieser Monat"
          , htmlSpanTextNode "" " findet unter anderem die gesuchten Daten im FH Wedel Kalender. Die dort aufgelisteten Daten k\246nnen direkt angeklickt werden um zu den entsprechenden Unterseiten zu gelangen."
          ]
        , htmlListItem "" $
          X.Element (T.pack "div")
          [ (T.pack "class", T.pack "example") ]
          [ htmlSpanTextNode "" "Die Suche kann auf bestimmte URLs eingeschränkt werden: "
          , htmlLink "" "querypage?query=href:vorlesungen/java%20AND%20Stack&button=Suchen" "href:vorlesungen/java AND Stack"
          , htmlSpanTextNode "" " sucht in allen Dokumenten, deren URL den Teilpfad "
          , htmlSpanTextNode "green" "vorlesungen/java"
          , htmlSpanTextNode ""      " enthält, nach dem Wort "
          , htmlSpanTextNode "green" "Stack"
          , htmlSpanTextNode ""      ". Hiermit kann die Suche zum Beispiel auf Unterlagen zu einer bestimmten Vorlesung eingeschr\228nkt werden."
          ]
        ]
      ]
