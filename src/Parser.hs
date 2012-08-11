{- |
Module      :  CurryIndexer
Description :  Modified parser for the search engine.
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

The module holds the functionalty for parsing the search string to a query. To narrow down the search, a special syntax can be used (i.e. ":function map" searches only for functions with the name "map").
-}

module Parser (parse) where

import Data.Char (ord)
-- import Data.Functor.Identity (Identity)

import Control.Applicative ((<*>), (<$>), (<|>), (*>), (<*))
import Control.Monad (guard)

import Text.Parsec.Token
import Text.Parsec.Prim  (runP, Parsec, try, many, parserReturn)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr (buildExpressionParser, Operator (..), Assoc (..))
import Text.Parsec.Combinator (sepBy1, notFollowedBy, option)
import Text.Parsec.Char (upper, alphaNum, lower, anyChar, oneOf, string)

import Holumbus.Query.Language.Grammar

import CurryInfo
import Helpers (showType)

infixr 4 -->

_moduleSpecifierName :: String
_moduleSpecifierName = ":module"

_functionSpecifierName :: String
_functionSpecifierName = ":function"

_typeSpecifierName :: String
_typeSpecifierName = ":type"

_signatureSpecifierName :: String
_signatureSpecifierName = ":signature"

_nondetSpecifierName :: String
_nondetSpecifierName = ":nondet"

_detSpecifierName :: String
_detSpecifierName = ":det"

_flexibleSpecifierName :: String
_flexibleSpecifierName = ":flexible"

_rigidSpecifierName :: String
_rigidSpecifierName = ":rigid"

_moduleSpecifierNameShort :: String
_moduleSpecifierNameShort = ":m"

_functionSpecifierNameShort :: String
_functionSpecifierNameShort = ":f"

_typeSpecifierNameShort :: String
_typeSpecifierNameShort = ":t"

_signatureSpecifierNameShort :: String
_signatureSpecifierNameShort = ":s"

_nondetSpecifierNameShort :: String
_nondetSpecifierNameShort = ":nd"

_detSpecifierNameShort :: String
_detSpecifierNameShort = ":d"

_flexibleSpecifierNameShort :: String
_flexibleSpecifierNameShort = ":fl"

_rigidSpecifierNameShort :: String
_rigidSpecifierNameShort = ":ri"

--------------------------
-- the signature parser --
--------------------------

-- | Parses type varibales (i.e. a character) and following whitespaces due to lexeme.
varParser :: Bool -> TypeExprParser
varParser inAListOrTuple = 
  var <$> (guard inAListOrTuple >> 
   (lexeme signatureTokenParser lower 
    <* notFollowedBy (identifier signatureTokenParser)))
  <|> var <$> lexeme signatureTokenParser lower <* notFollowedBy anyChar

-- | Parses primitive (unary) types (i.e. Int, Float, Bool ...) and following whitespaces due to identifier that is provided by signatureParser.
primParser :: TypeExprParser
primParser = 
  prim <$> identifier signatureTokenParser

-- | Parses a list.
listParser :: TypeExprParser
listParser = 
  try ((\texpr -> cons "[]" [texpr]) <$> brackets signatureTokenParser (signatureParser True))
  -- <|> (\_ -> cons "[]" []) <$> string "[]"

-- | Parses a tuple.
tupleParser :: TypeExprParser
tupleParser =
  try ((\tuple -> cons (tupleCons tuple) tuple) 
       <$> parens signatureTokenParser parseTuple)
  -- <|> (\_ -> cons "()" []) <$> string "()"
  where tupleCons list = "(" ++ replicate (length list - 1) ',' ++ ")"
        parseTuple = (\item _ itemList -> item:itemList) <$> 
                     (signatureParser True) <*> symbol signatureTokenParser "," 
                     <*> sepBy1 (signatureParser True) (symbol signatureTokenParser ",")

-- | Parses a type constructor. 
--   A whitespace is used as identicator, if it is followed by another successfull call of the signatureParser.
consParser :: TypeExprParser
consParser = 
  ((\constr _ expr -> cons constr expr) 
   <$> (identifier signatureTokenParser)
   <*> whiteSpace signatureTokenParser 
   <*> sepBy1 (signatureTerm False False) (whiteSpace signatureTokenParser))
  <|> (\constr -> cons constr []) <$> (string ":" <|> string "[]" <|> string "()")

-- | All possible forms of signatures. The boolean value indicates, if a type constructor may appear without parentheses.
signatureTerm :: Bool -> Bool -> TypeExprParser
signatureTerm allowConsParser inAListOrTuple = 
  (guard allowConsParser >> try consParser)
  <|> try tupleParser 
  <|> parens signatureTokenParser (signatureParser False)
  <|> listParser
  <|> primParser
  <|> (varParser inAListOrTuple)

-- helper function to define a general way for handling binary operators
binary :: GenTokenParser s u m -> String -> (a -> a -> a)
          -> Assoc -> Operator s u m a
binary parser name fun = 
  Infix ((\_ -> fun) <$> lexeme parser 
                                (reservedOp parser name))

-- Defines the binary "->"-operator used in the signature parser. 
-- It has a right associativity and returns the partial application "FuncType".
-- signatureTable :: [[OperatorTable TypeExpr]]
signatureTable = [[binary signatureTokenParser "->" (-->) AssocRight]]

-- | Buils an expression parser for signatures with the given term (signatureTerm) and table (signatureTable). 
signatureParser :: Bool -> TypeExprParser
signatureParser inAListOrTuple = 
  buildExpressionParser signatureTable (signatureTerm True inAListOrTuple)

--------------------------
-- the specifier parser --
--------------------------

-- Helper function to build a parser that parses a given specifier name (or its short name) an returns a query as specifier (i.e. Specifier [spec] identifier). The boolean value indicates if a following identifier is allowed.
aSpecifierParser :: Bool -> String -> String -> Parsec String String String -> QueryParser
aSpecifierParser optionalIdent spec short parser=
  build [spec] spec
  <|> build [spec] short
 where build specList str = 
        specify specList <$> (reservedOp specifierTokenParser str *>
         if optionalIdent then option ("") parser
                          else parserReturn "")

-- | Parses a moduleSpecifierName followed by an identifier (i.e. ":module Prelude").
moduleSpecifier :: QueryParser
moduleSpecifier = 
  aSpecifierParser True _moduleSpecifierName _moduleSpecifierNameShort 
   (identifier specifierTokenParser)

-- | Parses a functionSpecifierName followed by an identifier (i.e. ":function map").
--   It can also parse operators and parenthesized operators (i.e. (+) or +).
functionSpecifier :: QueryParser
functionSpecifier = 
  aSpecifierParser True _functionSpecifierName _functionSpecifierNameShort
   (identifier specifierTokenParser 
    <|> operator binaryTokenParser 
    <|> parens specifierTokenParser (operator binaryTokenParser))

-- | Parses a typeSpecifierName followed by an identifier (i.e. ":type Either").
--   It can also parse ":" and "[]" as they are special constructors.
typeSpecifier :: QueryParser
typeSpecifier = 
  aSpecifierParser True _typeSpecifierName  _typeSpecifierNameShort
   (identifier specifierTokenParser <|> string ":" <|> string "[]")

-- | Parses a nondetSpecifierName.
nondeterminismSpecifier :: QueryParser
nondeterminismSpecifier = 
  aSpecifierParser False _nondetSpecifierName _nondetSpecifierNameShort
   (identifier specifierTokenParser)

-- | Parses a detSpecifierName.
determinismSpecifier :: QueryParser
determinismSpecifier = 
  aSpecifierParser False _detSpecifierName _detSpecifierNameShort
   (identifier specifierTokenParser)

-- | Parses a flexibleSpecifierName.
flexibleSpecifier :: QueryParser
flexibleSpecifier = 
  aSpecifierParser False _flexibleSpecifierName _flexibleSpecifierNameShort
   (identifier specifierTokenParser)

-- | Parses a rigidSpecifierName.
rigidSpecifier :: QueryParser
rigidSpecifier = 
  aSpecifierParser False _rigidSpecifierName _rigidSpecifierNameShort
   (identifier specifierTokenParser)

-- | Parses a signatureSpecifier followed by a valid signature (i.e. ":signature Int->Int).
signatureSpecifier :: QueryParser
signatureSpecifier = 
  signatureSpecifier' _signatureSpecifierName <|> signatureSpecifier' _signatureSpecifierNameShort
 where signatureSpecifier' name =
        (\sig -> specify [_signatureSpecifierName] (testShow sig))
        <$> (reservedOp specifierTokenParser name *> signatureParser False)

-- | All possible forms of a specifier.
specifierParser :: QueryParser
specifierParser =
  try $ parens specifierTokenParser specifierParser
  <|> moduleSpecifier
  <|> typeSpecifier
  <|> nondeterminismSpecifier
  <|> determinismSpecifier
  <|> flexibleSpecifier
  <|> rigidSpecifier
  <|> functionSpecifier
  <|> signatureSpecifier

----------------------
-- the query parser --
----------------------

-- -- | Parses the permutation of specifiers, identifiers and signatures. 
-- permutations :: QueryParser
-- permutations = 
--   try ((\a -> binQuery a) <$> many (specifierParser 
--   <|> (Word <$> identifier binaryTokenParser)
--   <|> (Word <$> operator binaryTokenParser)
--   <|> ((\word -> specify [":signature"] (testShow word)) 
--                  <$> signatureParser)))

-- | Parses all possible forms of a user query: a specifier or the whole query with parentheses or permutation of specifiers, identifiers and signatures.
binOpTerm :: QueryParser
binOpTerm =
  try ((\a -> binQuery a) <$> 
   many (specifierParser 
         <|> (Word <$> identifier binaryTokenParser)
         <|> (Word <$> operator binaryTokenParser)
         <|> ((\word -> specify [":signature"] (testShow word)) 
                         <$> signatureParser False)))
  <|> parens binaryTokenParser binOpParser
  <|> parens specifierTokenParser specifierParser

-- | Defines the binary operators "AND", "OR", and "NOT". The order in the table represents the precendens (first = highest). When parsing a operator, it returns the partial application of "BinQuery" (and the given operator).
binOpTable = [[binary binaryTokenParser "AND" (BinQuery And) AssocLeft], 
              [binary binaryTokenParser "OR" (BinQuery Or) AssocLeft], 
              [binary binaryTokenParser "NOT" (BinQuery But) AssocLeft]]

-- | Builds an expression parser for the given table (binOpTable) and term (binOpTerm).
binOpParser :: QueryParser
binOpParser = buildExpressionParser binOpTable binOpTerm

-- |  Top-level parser that is used to parse the query the user is searching for. 
queryParser :: QueryParser
queryParser = whiteSpace binaryTokenParser *> binOpParser

-- | Runs the parser, the only exported function.
parse :: String -> Either String Query
parse = result . runP queryParser "" "curr(y)gle"
 where result (Left err) = Left (show err)
       result (Right q)  = Right q

---------------------------------------------------------------------------------------
{- The expression makeTokenParser language creates a GenTokenParser record that contains lexical parsers that are defined using the definitions in the language record (i.e. identifier, which also fails on reserved names). -}
---------------------------------------------------------------------------------------

signatureDef :: LanguageDef String
signatureDef = 
  emptyDef 
  { identStart      = upper,
    identLetter     = alphaNum,
    -- opStart         = opLetter signatureDef,
    -- opLetter        = oneOf ":[]()",
    -- reservedOpNames = [":", "[]", "()"],
    reservedNames   = 
     ["AND", "NOT", "OR", ":signature", ":module", ":function",":type", 
      ":nondet", ":det", ":flexible", ":rigid"]
   }

signatureTokenParser :: TokenParser String
signatureTokenParser = makeTokenParser signatureDef

specifierDef :: LanguageDef String
specifierDef = 
  emptyDef
  { identStart      = alphaNum,
    identLetter     = alphaNum,
    reservedNames   = ["AND","NOT","OR"]
  }

specifierTokenParser :: TokenParser String
specifierTokenParser = makeTokenParser specifierDef

binOpDef :: LanguageDef String
binOpDef = 
  emptyDef 
  { identStart      = lower,
    identLetter     = alphaNum,
    opStart         = opLetter binOpDef,
    opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~_",
    reservedOpNames = ["->",":"],
    reservedNames   = 
     ["AND", "NOT", "OR", ":signature", ":module", ":function",":type", 
      ":nondet", ":det", ":flexible", ":rigid"]
  }

binaryTokenParser :: TokenParser String
binaryTokenParser = makeTokenParser binOpDef

----------------------------------
--  some shortcut constructors  --
----------------------------------

var :: Char -> TypeExpr
var chr = TVar (ord chr - 97)

cons :: String -> [TypeExpr] -> TypeExpr
cons str = TCons ("", str)

(-->) :: TypeExpr -> TypeExpr -> TypeExpr
(-->) = FuncType

prim :: String -> TypeExpr
prim str = TCons ("",str) []

specify :: [String] -> String -> Query
specify specs str = Specifier specs (Word str)

-------------------------
-- shortcut type names --
-------------------------

type TypeExprParser = Parsec String String TypeExpr
type QueryParser    = Parsec String String Query
-- type OperatorTable a = Operator String String Identity a

-- shortcut prettyprint
testShow :: TypeExpr -> String
testShow = showType "" False

-- Converts a list of queries one query by folding left.
binQuery :: [Query] -> Query
binQuery [] = Word ""
binQuery (q:qs) = foldl (BinQuery And) q qs