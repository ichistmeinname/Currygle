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

--------------------------------------------------------------------------------
------------------------------     HTML STUFF     ------------------------------
--------------------------------------------------------------------------------

-- | Number of words contained in the teaser of the description text.
_numTeaserWords :: Int
_numTeaserWords = 30

-- Constant for the pagination label
_previousPage :: String 
_previousPage = "Prev"

-- Constant for the pagination label 
_nextPage :: String
_nextPage = "Next"

htmlTextNode :: String -> X.Node
htmlTextNode text = X.TextNode $ T.pack text

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

-- Flexible html construction (like <attribute class="css"> </> or <a href=link class=link> </a>
makeHtml :: String -> String -> String -> [X.Node] -> X.Node
makeHtml attribute link css =
  X.Element (T.pack attribute) (href ++ cssClass)
 where cssClass = eitherOr css [] [(T.pack "class", T.pack css)]
       href     = eitherOr link [] [(T.pack "href", T.pack link)]
       eitherOr str nothing result = if str == "" then nothing else result

-- If a given list is empty, return the empty list and return content otherwise
ifNotEmpty :: Eq a => [a] -> [b] -> [b]
ifNotEmpty list content = if list == [] then [] else content

-- Distinguishes between the text for a module and a author 
modOrAuthorText :: String -> String
modOrAuthorText othersAttribute
  | othersAttribute == "module" = "defined in "
  | othersAttribute == "author" = "coded by "
  | otherwise                   = ""

-- Returns the HTML node for the result's caption (<h3>)
captionHtml :: String -> [X.Node]
captionHtml text = [htmlh3 ([icon []] ++ [htmlTextNode text])]

-- Creates the HTML list item for a signature
sigHtml :: [X.Node] -> [X.Node]
sigHtml content = [htmlLiClass "signature" content]

-- Creates the HTML node for one result item
resultHtml :: String -> [X.Node] -> X.Node
resultHtml uri result = htmlDiv "" [htmlHref uri "result-item" result]

-- Creates the HTML text node for the contructors of a data
typeConsHtml :: [String] -> X.Node
typeConsHtml constr = htmlTextNode ("Constructors: " ++ intercalate ", " constr)
            
-- Creates the HTML list item for a given curryInfo description text
descriptionHtml :: String -> [X.Node]
descriptionHtml text = [htmlLiClass "description" [htmlTextNode descr]]
 where descr = if (length $ words text) > _numTeaserWords then teaserText ++ " ..."
                  else teaserText
       teaserText = unwords $ take _numTeaserWords $ words text

-- Creates one "document" of the search results as HTML node
-- Short example:
-- <div><a href=uri class="result-item"><h3>TITLE</h3><ul><li class="description>DESCR</li></ul></a></div>
makeResult :: String -> String -> (String, String) -> String -> [(String, String)] -> X.Node
makeResult title uri (oAtt, oText) descr sig =
  resultHtml uri (captionHtml title ++ 
                 [htmlUl (sigHtml signature ++ othersHtml ++ descriptionHtml descr)])
 where signature = ifNotEmpty sig [typeConsHtml $ map (\(name, consSig) -> name ++ " :: " ++ consSig) sig]
       othersHtml = ifNotEmpty oText [htmlLiClass oAtt [htmlTextNode $ modOrAuthorText oAtt ++ oText]]

-- Creates a htmt list item (possibly with a class attribute) with a html link
-- <li class="active"><a href=$link>$pageNumber</a></li>
pageLinks :: String -> Int -> Int -> X.Node
pageLinks query actPage pageNumber =
  htmlLiClass cssStyle [htmlHref (paginationLink query pageNumber) "" [htmlTextNode $ show pageNumber]]
 where cssStyle = if actPage == pageNumber then "active" else ""
       
-- Concats the link for the pagination for a given query and page
paginationLink :: String -> Int -> String
paginationLink query nr = "/querypage?query=" ++ query ++ "&page=" ++ show nr

-- Creates the pagination
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

-- Returns the pagination as HTML node that is used in the pager splice.
mkPagerLink :: String -> Int -> Int -> [X.Node]
mkPagerLink query actPage number =
  [htmlDiv "pagination pagination-centered" 
    [htmlUl (pagination query actPage number)]]

--------------------------------------------------------------------------------
------------------------------  Pretty  Printing  ------------------------------
--------------------------------------------------------------------------------

-- When types are used in the module where they're defined, a qualified name is not necessary,
-- same holds for types of the prelude.
isQualifiedName :: String -> String -> Bool
isQualifiedName moduleName fModuleName = moduleName == fModuleName || fModuleName == "Prelude"

-- Returns a given function with its qualfied name, if necessary
qualifiedName :: String -> String -> String -> String
qualifiedName moduleName fModuleName funcName = 
  if isQualifiedName moduleName fModuleName then funcName else moduleName ++ "." ++ funcName

-- Parens a given string when flag is true
paren :: Bool -> String -> String
paren parens str 
    | parens    = "(" ++ str ++ ")"
    | otherwise = str 

-- Pretty printing for special types like lists, tupels or type variables
prettyPrintSpecialType :: String -> String -> [TypeExpr] -> String
prettyPrintSpecialType _ name [] = name
prettyPrintSpecialType name funcName [tExpr] 
    | funcName == "[]" = "[" ++  (prettyPrint name tExpr) ++ "]"
    | otherwise = funcName ++ " " ++ (prettyPrint name tExpr)
prettyPrintSpecialType name funcName tExprList@(_:_:_)  
    | head funcName == '(' =
    "(" ++ intercalate "," (map (prettyPrint name) tExprList) ++ ")"
    | otherwise = funcName ++ " " ++ intercalate " " (map (prettyPrint name) tExprList)

-- Pretty printing for function types
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

-- Converts a TypeExpr to a list of Strings
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

-- Pretty printing for signatures (i.e. ["Int", "Int"] -> "Int->Int")
listToSignature :: [String] -> String
listToSignature = intercalate "->"

--------------------------------------------------------------------------------
------------------------------   miscellaneous    ------------------------------
--------------------------------------------------------------------------------

-- Splits strings at whitespaces (ex.: for better handling of queries)
splitOnWhitespace :: String -> [String]
splitOnWhitespace = splitStringOn " "

-- Shortcut to split on "->" (ex.: for better handling of signatures)
splitOnArrow :: String -> [String]
splitOnArrow = splitStringOn "->"

-- Splits string on a given string
splitStringOn :: String -> String -> [String]
splitStringOn splitter text = map T.unpack (T.splitOn  (T.pack splitter) (T.pack text))

-- returns a path that ends with '/'
fullPath :: String -> String
fullPath path  = if last path == '/' then path else path ++ "/"

-- Words shorter than 2 characters are biased
biasedWord :: String -> Bool
biasedWord s = length s < 3

-- Converts a list of indices to a string of variables
varIndex :: [Int] -> String
varIndex = (++) " " . intercalate " " . map (\index -> [chr (97+index)])

-- Converts a String to an Int, returns defaultValue if conversion fails.
strToInt :: Int -> String -> Int
strToInt defaultValue str
  | length readsResult > 0 = fst $ head readsResult
  | otherwise = defaultValue
  where
  readsResult = reads str




