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

module Helpers where

import qualified Data.Text as T (pack, splitOn, unpack)
import           Data.List
  (intercalate, isInfixOf, isPrefixOf, isSuffixOf, tails)
import           Data.Char      (chr)

import           CurryInfo

-- ---------------------------------------------------------------------------
-- Pretty Printing
--- --------------------------------------------------------------------------

-- When types are used in the module where they're defined, a qualified name is not necessary,
-- same holds for types of the prelude.
isQualifiedName :: String -> String -> Bool
isQualifiedName moduleName fModuleName = moduleName == fModuleName || fModuleName == "Prelude" || moduleName == ""

-- Returns a given function with its qualfied name, if necessary
qualifiedName :: String -> String -> String -> String
qualifiedName moduleName fModuleName funcName =
  if isQualifiedName moduleName fModuleName then funcName else fModuleName ++ "." ++ funcName

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
 | isString tc $ head ts=
     "String"
 | isList tc =
     "[" ++ showType modName False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(," =                        -- tuple type
     "(" ++ intercalate "," (map (showType modName False) ts) ++ ")"
 | otherwise =
     paren nested
      (showTypeCons modName tc ++ " " ++
       intercalate " " (map (showType modName True) ts))
showType _ _ _ = ""


resultType :: TypeInfo -> TypeExpr
resultType (TypeInfo typeName _ typeVars moduleName _ _) =
  TCons (moduleName, typeName) (map TVar typeVars)

--
constrTypeExpr :: TypeInfo -> [(String, TypeExpr)]
constrTypeExpr tInfo@(TypeInfo _ constrs _ _ _ typSyn) =
  map (\((_,cName), argTypes) -> (cName,if typSyn then foldr1 FuncType argTypes
                                            else foldr FuncType (resultType tInfo) argTypes)) constrs

-- showTypeList :: String -> (QName, [TypeExpr]) -> ([String], String)
-- showTypeList typeName ((modName, consName), tExprList) =
--   ( map (\expr -> (showType modName False expr) ++ (" -> " ++ typeName)) tExprList, consName)

splitType :: Bool -> TypeExpr -> [String]
splitType _ (TVar i) = [[chr (97+i)]]
splitType nested (FuncType t1 t2) =
   paren nested
    (showType "" (isFunctionType t1) t1) : (splitType False t2)
splitType nested (TCons tc ts)
 | null ts = [showTypeCons "" tc]
 | isString tc $ head ts =
     ["String"]
 | isList tc =
     ["[" ++ showType "" False (head ts) ++ "]"] -- list type
 | take 2 (snd tc) == "(," =                        -- tuple type
     ["(" ++ intercalate "," (map (showType "" False) ts) ++ ")"]
 | otherwise =
     [paren nested
      (showTypeCons "" tc ++ " " ++
       intercalate " " (map (showType "" True) ts))]
splitType _ _ = [""]

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

signatureComponents :: TypeExpr -> [String]
signatureComponents expr = map listToSignature (partA ++ partB)
 where partA = map removeEmptyStrings $ init $ tails $ splitType False expr
       partB =
         if -- ("(" `isInfixOf` (concat $ last partA) || "[" `isInfixOf` (concat $ last partA)) &&
            not ("()" `isInfixOf` (concat $ last partA))
            then redundantParens [removeEmptyStrings $ tail $ concatMap searchForParens $ map splitOnWhitespace $ last partA]
            else []


isString :: (String, String) -> TypeExpr -> Bool
isString qName (TCons ("Prelude","Char") []) = isList qName
isString qName (TCons ("","Char")        []) = isList qName
isString _ _                                 = False

isList :: (String, String) -> Bool
isList qName = (qName == ("Prelude","[]") || qName == ("","[]"))

isFunctionType :: TypeExpr -> Bool
isFunctionType (FuncType _ _) = True
isFunctionType _              = False

showTypeCons :: String -> (String, String) -> String
showTypeCons modName (mtc,tc) =
  qualifiedName modName mtc tc

consSignature :: [TypeExpr] -> [String]
consSignature = concatMap (\tExpr -> signatureComponents tExpr)

consToList :: String -> (QName, [TypeExpr]) -> ([String], String)
consToList typeName ((_, consName), tExprList) =
  ((consSignature tExprList) ++ [typeName], consName)

-- Pretty printing for signatures (i.e. ["Int", "Int"] -> "Int->Int")
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
