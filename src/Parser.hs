module Parser where

import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Language.Parser
import           Data.List
import           Data.Char
import           Data.Maybe
import           Helpers

import Debug.Trace (trace)


prepareQuery :: String -> Either String Query
prepareQuery s
    | customized s = trace ("WTF is the BinQuery? " ++ show (customQuery s)) (Right $ customQuery s)
    | otherwise    = parseQuery s

specify :: [String] -> String -> Query
specify names str = Specifier names $ Word str

customQuery :: String -> Query
customQuery s = customize $ splitOnWhitespace s
  where customize :: [String] -> Query
        customize [] = Word ""
        customize [x] = custom x
        customize (x:xs) = BinQuery And (custom x) (customize xs)
        
        custom x
          | funcSpecifier x  = specify ["Function"] (strip "function:" x)
          | typeSpecifier x  = specify ["Type"] (strip "type:" x)
          | modSpecifier x   = specify ["Module"] (strip "module:" x)
          -- | inSpecifier x   = specify ["Module"] (strip "in:" x)
          | signature x      = specify ["Signature"] (strip "signature:" x)
          | ndSpecifier x    = specify ["NonDet"] (strip "nd" x)
          | detSpecifier x   = specify ["Det"] (strip "det" x)
          | flexSpecifier x  = specify ["Flex"] (strip "flex" x)
          | rigidSpecifier x = specify ["Rigid"] (strip "rig" x)
          | caseSensitive x  = specify ["Type","TheModule"] x
          | otherwise        = Word x

customized :: String -> Bool
customized s = funcSpecifier s || typeSpecifier s || modSpecifier s || sigSpecifier s || caseSensitive s


-- customSignature :: String -> Query
-- customSignature s = signature $ splitOnWhitespace s
--   where signature :: [String] -> Query 
--         signature []     = Word ""
--         signature (x:xs) = if isSignature x 
--                              then BinQuery And (specify ["Signature"] $ Word x) (signature xs)
--                              else BinQuery And (Word x) (signature xs)

caseSensitive :: String -> Bool
caseSensitive [] = False
caseSensitive (x:_) = isUpper x

funcSpecifier :: String -> Bool
funcSpecifier = isPrefixOf "function:"

typeSpecifier :: String -> Bool
typeSpecifier = isPrefixOf "type:"

modSpecifier :: String -> Bool
modSpecifier = isPrefixOf "module:"

ndSpecifier :: String -> Bool
ndSpecifier = isPrefixOf "nd"

detSpecifier :: String -> Bool
detSpecifier = isPrefixOf "det"

flexSpecifier :: String -> Bool
flexSpecifier = isPrefixOf "flex"

rigidSpecifier :: String -> Bool
rigidSpecifier = isPrefixOf "rig"

-- inSpecifier :: String -> Bool
-- inSpecifier = isPrefixOf "in:"

strip :: String -> String -> String
strip prefix s = 
    if isPrefixOf prefix str then fromJust $ stripPrefix prefix str
                           else s
  where str = concat $ splitOnWhitespace s

-- Implicit search for signature when string includes "->"
isSignature :: String -> Bool
isSignature = isInfixOf "->"

-- Explicit use of search extension "signature:"
sigSpecifier :: String -> Bool
sigSpecifier = isPrefixOf "signature:"

signature :: String -> Bool
signature s = isSignature s || sigSpecifier s 

stripSignature :: String -> String
stripSignature = strip "signature:"

