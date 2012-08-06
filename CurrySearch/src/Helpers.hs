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
paginationLink query nr = "/currygle?query=" ++ query ++ "&page=" ++ show nr

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

-- Pretty printing for signatures
showType :: String -> Bool -> TypeExpr -> String
showType _ _ (TVar i) = [chr (97+i)]
showType modName nested (FuncType t1 t2) =
   paren nested
    (showType modName (isFunctionType t1) t1 ++ " -> " ++ showType modName False t2)
showType modName nested (TCons tc ts)
 | null ts = showTypeCons modName tc
 | (tc == ("Prelude","[]") || tc == ("","[]")) && (isString $ head ts) = 
     "String"
 | tc == ("Prelude","[]") || tc == ("","[]") =
     "[" ++ showType modName False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(," =                        -- tuple type
     "(" ++ intercalate "," (map (showType modName False) ts) ++ ")"
 | otherwise = 
     paren nested
      (showTypeCons modName tc ++ " " ++
       intercalate " " (map (showType modName True) ts))
showType _ _ _ = ""

showTypeList :: String -> (QName, [TypeExpr]) -> ([String], String)
showTypeList typeName ((modName, consName), tExprList) =
  ( map (\expr -> (showType modName False expr) ++ (" -> " ++ typeName)) tExprList, consName)

splitType :: String -> Bool -> TypeExpr -> [String]
splitType _ _ (TVar i) = [[chr (97+i)]]
splitType modName nested (FuncType t1 t2) =
   paren nested
    (showType modName (isFunctionType t1) t1) : (splitType modName False t2)
splitType modName nested (TCons tc ts)
 | null ts = [showTypeCons modName tc]
 | tc==("Prelude","[]") && (isString $ head ts) = 
     ["String"]
 | tc==("Prelude","[]") =
     ["[" ++ showType modName False (head ts) ++ "]"] -- list type
 | take 2 (snd tc) == "(," =                        -- tuple type
     ["(" ++ intercalate "," (map (showType modName False) ts) ++ ")"]
 | otherwise = 
     [paren nested
      (showTypeCons modName tc ++ " " ++
       intercalate " " (map (showType modName True) ts))]
splitType _ _ _ = [""]

searchForParens :: [String] -> [String]
searchForParens (x:xs) = 
  if "(" `isPrefixOf` x then endParen (x, xs) else x : searchForParens xs
 where endParen (parenString, (y:ys)) = 
         if ")" `isSuffixOf` y then (parenString ++ " " ++ y) : ys
                               else endParen (parenString++ " " ++ y, ys)
       endParen (p, []) = [p]
searchForParens [] = [[]]

redundantParens :: [[String]] -> [[String]]
redundantParens [[x]]
  | head x == '(' = [[take (length x - 2) $ drop 1 x]]
redundantParens (x:ys) = x : redundantParens ys
redundantParens x      = x

removeEmptyStrings :: [String] -> [String]
removeEmptyStrings = filter (\x -> not $ x == "")

signatureComponents :: (QName, TypeExpr) -> [String]
signatureComponents ((modName, _), expr) = map listToSignature (partA ++ partB)
 where partA = map removeEmptyStrings $ init $ tails $ splitType modName False expr
       partB = 
         if -- ("(" `isInfixOf` (concat $ last partA) || "[" `isInfixOf` (concat $ last partA)) && 
            not ("()" `isInfixOf` (concat $ last partA))
            then redundantParens [removeEmptyStrings $ tail $ concatMap searchForParens $ map splitOnWhitespace $ last partA] 
            else []

isString :: TypeExpr -> Bool
isString (TCons ("Prelude","Char") []) = True
isString (TCons ("","Char")        []) = True 
isString _                             = False

isFunctionType :: TypeExpr -> Bool
isFunctionType (FuncType _ _) = True
isFunctionType _              = False

showTypeCons :: String -> (String, String) -> String
showTypeCons modName (mtc,tc) =
  qualifiedName modName mtc tc

consSignature :: String -> [TypeExpr] -> [String]
consSignature modName = concatMap (\tExpr -> signatureComponents ((modName,""), tExpr))

consToList :: String -> (QName, [TypeExpr]) -> ([String], String)
consToList typeName ((modName, consName), tExprList) = 
  ((consSignature modName tExprList) ++ [typeName], consName)

-- -- Pretty printing for signatures (i.e. ["Int", "Int"] -> "Int->Int")
listToSignature :: [String] -> String
listToSignature = intercalate " -> "

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

-- -- returns a path that ends with '/'
-- fullPath :: String -> String
-- fullPath path  = if hasTrailingPathSeperator then path else path ++ "/"

-- Words shorter than 2 characters are biased
biasedWord :: String -> Bool
biasedWord s = length s < 3

-- Converts a list of indices to a string of variables
varIndex :: [Int] -> String
varIndex [] = ""
varIndex varIndices = (++) " " $ intercalate " " $ map (\index -> [chr (97+index)]) varIndices

-- Converts a String to an Int, returns defaultValue if conversion fails.
strToInt :: Int -> String -> Int
strToInt defaultValue str
  | length readsResult > 0 = fst $ head readsResult
  | otherwise = defaultValue
  where
  readsResult = reads str




