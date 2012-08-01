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

import Text.Parsec.Token
import Text.Parsec.Prim  (runP, Parsec, try, many, parserZero, parserReturn)
import Text.Parsec.Perm
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr (buildExpressionParser, Operator (..), Assoc (..))
import Text.Parsec.Combinator (sepBy1, notFollowedBy, option)
import Text.Parsec.Char (upper, alphaNum, lower, anyChar)

import Holumbus.Query.Language.Grammar

import CurryInfo
import Helpers (showType)

import Debug.Trace (trace)

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
varParser :: TypeExprParser
varParser = var <$> lexeme signatureTokenParser lower <* notFollowedBy anyChar

-- | Parses primitive (unary) types (i.e. Int, Float, Bool ...) and following whitespaces due to identifier that is provided by signatureParser.
primParser :: TypeExprParser
primParser = prim <$> identifier signatureTokenParser

-- | Parses a list.
listParser :: TypeExprParser
listParser = 
  (\texpr -> cons "[]" [texpr]) <$> brackets signatureTokenParser signatureParser

-- | Parses a tuple.
tupleParser :: TypeExprParser
tupleParser =
  try ((\tuple -> cons (tupleCons tuple) tuple) 
       <$> parens signatureTokenParser parseTuple)
  where tupleCons list = "(" ++ replicate (length list - 1) ',' ++ ")"
        parseTuple = (\item _ itemList -> item:itemList) <$> 
                     signatureParser <*> symbol signatureTokenParser "," 
                     <*> sepBy1 signatureParser (symbol signatureTokenParser ",")

-- | Parses a type constructor. 
--   A whitespace is used as identicator, if it is followed by another successfull call of the signatureParser.
consParser :: TypeExprParser
consParser = 
  (\constr _ expr -> cons constr expr) 
   <$> identifier signatureTokenParser 
   <*> whiteSpace signatureTokenParser 
   <*> sepBy1 (signatureTerm False) (whiteSpace signatureTokenParser) 

-- | All possible forms of signatures. The boolean value indicates, if a type constructor may appear without parentheses.
signatureTerm :: Bool -> TypeExprParser
signatureTerm b = 
  (if b then try consParser else parserZero)
  <|> try tupleParser 
  <|> parens signatureTokenParser signatureParser
  <|> listParser
  <|> primParser
  <|> varParser

-- helper function to define a general way for handling binary operators
binary :: GenTokenParser s u m -> String -> (a -> a -> a)
          -> Assoc -> Operator s u m a
binary parser name fun = 
  Infix ((\_ -> fun) <$> lexeme parser 
                                (reservedOp parser name))

-- Defines the binary "->"-operator used in the signature parser. 
-- It has a right associativity and returns the partial application "FuncType".
-- signatureTable :: [[OperatorTable TypeExpr]]
signatureTable = [[binary signatureTokenParser "->" (FuncType) AssocRight]]

-- | Buils an expression parser for signatures with the given term (signatureTerm) and table (signatureTable). 
signatureParser :: TypeExprParser
signatureParser = buildExpressionParser signatureTable (signatureTerm True)

--------------------------
-- the specifier parser --
--------------------------

-- Helper function to build a parser that parses a given specifier name (or its short name) an returns a query as specifier (i.e. Specifier [spec] identifier). The boolean value indicates if a following identifier is allowed.
aSpecifierParser :: Bool -> String -> String -> QueryParser
aSpecifierParser optionalIdent spec short =
  build [spec] spec
  <|> build [spec] short
 where build specList str = 
        specify specList <$> (reservedOp specifierTokenParser str *>
         if optionalIdent then option ("") (identifier specifierTokenParser)
                          else parserReturn "")

-- | Parses a moduleSpecifierName followed by an identifier (i.e. ":module Prelude").
moduleSpecifier :: QueryParser
moduleSpecifier = 
  aSpecifierParser True _moduleSpecifierName _moduleSpecifierNameShort

-- | Parses a functionSpecifierName followed by an identifier (i.e. ":function map").
functionSpecifier :: QueryParser
functionSpecifier = 
  aSpecifierParser True _functionSpecifierName _functionSpecifierNameShort

-- | Parses a typeSpecifierName followed by an identifier (i.e. ":type Either").
typeSpecifier :: QueryParser
typeSpecifier = 
  aSpecifierParser True _typeSpecifierName  _typeSpecifierNameShort

-- | Parses a nondetSpecifierName.
nondeterminismSpecifier :: QueryParser
nondeterminismSpecifier = 
  aSpecifierParser False _nondetSpecifierName _nondetSpecifierNameShort

-- | Parses a detSpecifierName.
determinismSpecifier :: QueryParser
determinismSpecifier = 
  aSpecifierParser False _detSpecifierName _detSpecifierNameShort

-- | Parses a flexibleSpecifierName.
flexibleSpecifier :: QueryParser
flexibleSpecifier = 
  aSpecifierParser False _flexibleSpecifierName _flexibleSpecifierNameShort

-- | Parses a rigidSpecifierName.
rigidSpecifier :: QueryParser
rigidSpecifier = 
  aSpecifierParser False _rigidSpecifierName _rigidSpecifierNameShort

-- | Parses a signatureSpecifier followed by a valid signature (i.e. ":signature Int->Int).
signatureSpecifier :: QueryParser
signatureSpecifier = 
  (\sig -> specify [_signatureSpecifierName] (testShow sig)) <$> 
   (reservedOp specifierTokenParser _signatureSpecifierName *> signatureParser)
  <|> (\sig -> specify [_signatureSpecifierName] (testShow sig)) <$> 
  (reservedOp specifierTokenParser _signatureSpecifierNameShort *> signatureParser)

-- | All possible forms of a specifier.
specifierParser :: QueryParser
specifierParser =
  try $ parens specifierTokenParser specifierParser
  <|> moduleSpecifier
  <|> functionSpecifier
  <|> typeSpecifier
  <|> nondeterminismSpecifier
  <|> determinismSpecifier
  <|> flexibleSpecifier
  <|> rigidSpecifier
  <|> signatureSpecifier

----------------------
-- the query parser --
----------------------

-- | Parses the permutation of specifiers, identifiers and signatures. 
permutations :: QueryParser
permutations = 
  try $ permute ((\specs1 identOrSig specs2-> binQuery (specs1++identOrSig++specs2)) 
   <$$> many specifierParser
   <||> many (try (Word <$> (identifier binaryTokenParser))
             <|> try ((\word -> specify [":signature"] (testShow word)) 
                       <$> signatureParser))
   <||>  many specifierParser)


-- Remember: permutations will never fail because of the "many"-operator
-- | Parses all possible forms of a user query: a specifier or the whole query with parentheses or permutation of specifiers, identifiers and signatures.
binOpTerm :: QueryParser
binOpTerm =
  try (permutations *> parens specifierTokenParser specifierParser) 
  <|> try (permutations *> parens binaryTokenParser binOpParser)
  <|> permutations

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
       result (Right q)  = trace (show q) (Right q)

---------------------------------------------------------------------------------------
{- The expression makeTokenParser language creates a GenTokenParser record that contains lexical parsers that are defined using the definitions in the language record (i.e. identifier, which also fails on reserved names). -}
---------------------------------------------------------------------------------------

signatureDef :: LanguageDef String
signatureDef = 
  emptyDef 
  { identStart      = upper,
    identLetter     = alphaNum,
    reservedNames   = 
     ["AND", "NOT", "OR", ":signature", ":module", ":function",":type", 
      ":nondet", ":det", ":flexible", ":rigid", "->"]
   }

signatureTokenParser :: TokenParser String
signatureTokenParser = makeTokenParser signatureDef

specifierDef :: LanguageDef String
specifierDef = 
  emptyDef
  { identStart      = alphaNum,
    identLetter     = alphaNum,
    -- reservedOpNames = [":signature", ":module", ":function",":type"
    --                    ":nondet", ":det", ":flexible", ":rigid"],
    reservedNames   = ["AND","NOT","OR"]
  }

specifierTokenParser :: TokenParser String
specifierTokenParser = makeTokenParser specifierDef

binOpDef :: LanguageDef String
binOpDef = 
  emptyDef 
  { identStart      = lower,
    identLetter     = alphaNum
    -- reservedOpNames = ["AND", "OR", "NOT"],
    -- reservedNames   = ["->", ":signature", ":module", ":function",":type", 
    --                    ":nondet", ":det", ":flexible", ":rigid"]
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
