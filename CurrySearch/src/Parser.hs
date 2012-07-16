module Parser where

import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Language.Parser
import           Data.List
import           Data.Char
import           Data.Maybe
-- import           Data.Map as Map
import           Helpers

-- import Debug.Trace (trace)

-- import Debug.Trace (trace)

prepareQuery :: String -> Either String Query
prepareQuery s
    | customized s = customQuery s
    | otherwise    = parseQuery s

specify :: [String] -> String -> Query
specify names "" = BinQuery Or (Specifier names $ Word "") (Specifier names (Negation $ Word "")) 
-- an empty string query does not produce the wanted result
specify names str = Specifier names $ Word str

customQuery :: String -> Either String Query
customQuery s = Right $ customize $ splitOnWhitespace s
  where customize :: [String] -> Query
        customize [x] = if binary x then Word "" else custom x
        customize (x:y:xs) 
          | x == "NOT" = BinQuery But (Word "") (customize $ y:xs)
          | binary x = customize $ y:xs 
          | not (isSpecifier y || binary y) = customize $ (x++y):xs
          | binary y = binQuery y (custom x) $ customize xs
          | isSpecifier y = BinQuery And (custom x) (customize $ y:xs)
        customize _ = Word ""
        binary x = x `elem`["AND", "OR", "NOT"]
        

customized :: String -> Bool
customized s 
    | ":" `isPrefixOf` s = isSpecifier s
    | isSignature s      = True
    | caseSensitive s    = True
    | otherwise          = False

-- creates specified query for a string
custom :: String -> Query
custom x
    | funcSpecifier x  = specify ["Function"] (strip ":function" x)
    | typeSpecifier x  = specify ["Type"] (strip ":typ" x) 
    | modSpecifier x   = specify ["Module"] (strip ":module" x)
    | signature x      = specify ["Signature"] (strip ":signature" x)
    | ndSpecifier x    = specify ["NonDet"] (strip ":nondet" x)
    | detSpecifier x   = specify ["Det"] (strip ":det" x)
    | flexSpecifier x  = specify ["Flex"] (strip ":flex" x)
    | rigidSpecifier x = specify ["Rigid"] (strip ":rigid" x)
    | caseSensitive x  = specify ["Type","TheModule"] x
    | otherwise        = Word x

-- creates binary query for a string
binQuery :: String -> Query -> Query -> Query
binQuery x 
    | x == "AND"       = BinQuery And
    | x == "OR"        = BinQuery Or
    | otherwise        = BinQuery But

-- checks if a given string is a specifier
isSpecifier :: String -> Bool
isSpecifier s = any (`isPrefixOf` s) specifiers

caseSensitive :: String -> Bool
caseSensitive [] = False
caseSensitive (x:_) = isUpper x

-- normalizeSignature :: String -> String
-- -- (Int -> Char)-> [Int] -> [Char]
-- normalizeSignature s = trace (show (Prelude.map (\c -> (pairMap s) ! [c]) s)) (Prelude.map (\c -> (pairMap s) ! [c]) s) 
--   where pairMap = insertNext (Map.fromList []) (ord 'a') . splitOnArrow
--         insertNext pMap _ [] = pMap
--         insertNext pMap i (x:xs) = if Map.notMember x pMap then insertNext (Map.insert x (chr i) pMap) (i+1) xs else insertNext pMap i xs


-- list of all definied specifiers
specifiers :: [String] 
specifiers = [":function", ":type", ":module", ":nondet", 
              ":det", ":flex", ":rigid", ":signature"]

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

-- Implicit search for signature when string includes "->"
isSignature :: String -> Bool
isSignature = isInfixOf "->"

signature :: String -> Bool
signature s = isSignature s || sigSpecifier s 

strip :: String -> String -> String
strip prefix s = 
    if isPrefixOf prefix str then fromJust $ stripPrefix prefix str
                             else s
  where str = concat $ splitOnWhitespace s


