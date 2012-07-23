{- |
Module      :  CurryIndexer
Description :  Modified parser for the search engine.
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

The module holds the functionalty for parsing the search string to a query. To narrow down the search, a special syntax can be used (i.e. :function map searches only for functions with the name "map"). The processing of this syntax is handled in this module.
-}

module Parser (prepareQuery) where

import           Data.List
import           Data.Char
import           Data.Maybe

import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Language.Parser

import           Helpers

funcSpecifier :: String -> Bool
funcSpecifier = isPrefixOf ":function"

typeSpecifier :: String -> Bool
typeSpecifier = isPrefixOf ":type"

modSpecifier :: String -> Bool
modSpecifier = isPrefixOf ":module"

ndSpecifier :: String -> Bool
ndSpecifier = isPrefixOf ":nondet"

detSpecifier :: String -> Bool
detSpecifier = isPrefixOf ":det"

flexSpecifier :: String -> Bool
flexSpecifier = isPrefixOf ":flex"

rigidSpecifier :: String -> Bool
rigidSpecifier = isPrefixOf ":rigid"

sigSpecifier :: String -> Bool
sigSpecifier = isPrefixOf ":signature"

strip :: String -> String -> String
strip prefix s = 
    if isPrefixOf prefix str then fromJust $ stripPrefix prefix s
                             else s
  where str = concat $ splitOnWhitespace s

-- checks if a given string is a specifier
isSpecifier :: String -> Bool
isSpecifier s = any (`isPrefixOf` s) specifiers

-- list of all definied specifiers
specifiers :: [String] 
specifiers = [":function", ":type", ":module", ":nondet", 
              ":det", ":flex", ":rigid", ":signature"]

caseSensitive :: String -> Bool
caseSensitive [] = False
caseSensitive (x:_) = isUpper x

-- Implicit search for signature when string includes "->"
isSignature :: String -> Bool
isSignature = isInfixOf "->"

signature :: String -> Bool
signature s = isSignature s || sigSpecifier s 

-- Creates a Specifier Query 
-- Attention! An empty Word Query does not search for all words!
specify :: [String] -> String -> Query
specify names "" = BinQuery Or (Specifier names $ Word "") (Specifier names (Negation $ Word "")) 
-- an empty string query does not produce the wanted result
specify names str = Specifier names $ Word str

-- Creates specified query for a string depending on the specifier
custom :: String -> Query
custom x
    | funcSpecifier x  = specify ["Function"] (strip ":function" x)
    | typeSpecifier x  = specify ["Type"] (strip ":type" x) 
    | modSpecifier x   = specify ["Module"] (strip ":module" x)
    | signature x      = specify ["Signature"] (strip ":signature" x)
    | ndSpecifier x    = specify ["NonDet"] (strip ":nondet" x)
    | detSpecifier x   = specify ["Det"] (strip ":det" x)
    | flexSpecifier x  = specify ["Flex"] (strip ":flex" x)
    | rigidSpecifier x = specify ["Rigid"] (strip ":rigid" x)
    | caseSensitive x  = specify ["Type","TheModule"] x
    | otherwise        = Word x

-- Creates binary query for a string
binQuery :: String -> Query -> Query -> Query
binQuery x 
    | x == "AND"       = BinQuery And
    | x == "OR"        = BinQuery Or
    | otherwise        = BinQuery But

binary :: String -> Bool        
binary = flip elem ["AND", "OR", "NOT"] 

customize :: [String] -> Query
customize [x] = if binary x then Word "" else custom x
customize (x:y:xs) 
  | x == "NOT" = BinQuery But (Word "") (customize $ y:xs)
  -- | isSignature x = customize $ (":signature"++x):y:xs
  | binary x = customize $ y:xs 
  | "(" `isPrefixOf` y && ")" `isSuffixOf` y = customize $ (x ++ (strip "(" $ init y)) : xs
  | not (isSpecifier y || binary y) = customize $ (x++y) : xs
  | binary y = binQuery y (custom x) $ customize xs
  | isSpecifier y = BinQuery And (custom x) (customize $ y:xs)
customize (x:xs) 
  | isSignature x = custom $ ":signature"++x
  | otherwise = Word $ intercalate " " $ x:xs
customize [] = Word ""

isMonad :: String -> Bool
isMonad str = 
  any (flip isInfixOf $ string str) ["maybe", "io", "ioref"]
 where string = last . splitOnWhitespace . map toLower

searchForMonads :: [String] -> [String]
searchForMonads (x:y:xs) = 
 if isMonad x && not (isSpecifier y || binary y) then [x++" "++y] ++ searchForMonads xs
                                                 else x : searchForMonads (y:xs)
searchForMonads xs = xs

searchForParens :: [String] -> [String]
searchForParens (x:xs) = 
  if "(" `isPrefixOf` x then endParen (x, xs) else x : searchForParens xs
 where endParen (parenString, (y:ys)) = 
         if ")" `isSuffixOf` y then (parenString ++ y) : ys
                               else endParen (parenString++y, ys)
       endParen (p, []) = [p]
searchForParens [] = [[]]

searchForSignature :: [String] -> [String]
searchForSignature (x:y:xs) = 
  if y == "->" then endSignature ((x++y), xs) else x : searchForSignature (y:xs)
 where endSignature (sig, (sigComp:str:rest)) = 
         if not $ str == "->" then (sig ++ sigComp):str:rest 
                            else endSignature (sig++sigComp++str,rest)
       endSignature (sig, [end]) = [sig++end]
       endSignature (sig, []) = [sig]
searchForSignature xs = xs

-- Prepares a given string to be converted to a query (whitespaces, parens, monads have to be considered)
customQuery :: String -> Either String Query
customQuery s 
  | isSignature s = Right $ customize $ searchForSignature $ splitQuery s
  | otherwise     = Right $ customize $ splitQuery s
 where splitQuery = searchForParens . searchForMonads . fixForSignatures
       fixForSignatures = intercalate ["->"] . removeEmptyStrings . map splitOnWhitespace . splitOnArrow
       removeEmptyStrings = map (filter (\x -> not $ x == []))

customized :: String -> Bool
customized s 
    | ":" `isPrefixOf` s = isSpecifier s
    | isSignature s      = True
    | caseSensitive s    = True
    | otherwise          = False

-- | Distinguishes between a string with and without specifiers (like :signature) and processes it.
prepareQuery :: String -> Either String Query
prepareQuery s
    | customized s = customQuery s
    | otherwise    = parseQuery s