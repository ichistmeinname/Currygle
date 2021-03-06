{- |
Module      :  CurryIndexer
Description :  Modified parser for the search engine.
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

The module holds the functionalty for parsing the search string to a query.
To narrow down the search, a special syntax can be used
(i.e. ":function map" searches only for functions with the name "map").
-}

module QueryParser (parse) where

import Control.Applicative   ((<*>), (<$>), (<|>), (*>), (<*))
import Control.Monad         (guard)
import Data.Char             (ord)
import Data.Functor.Identity (Identity)

import Text.Parsec.Char       (upper, alphaNum, lower, oneOf, char)
import Text.Parsec.Combinator (sepBy1, option, many1, eof)
import Text.Parsec.Expr       (buildExpressionParser, Operator (..), Assoc (..))
import Text.Parsec.Language   (emptyDef)
import Text.Parsec.Prim       (runP, Parsec, try, many, parserReturn)
import Text.Parsec.Token

import Holumbus.Query.Language.Grammar

import CurryInfo
import Helpers (showType)

infixr 4 -->

-- Name for the specifier to restrict the search to module names.
_moduleSpecifierName :: String
_moduleSpecifierName = "module"

-- Name for the specifier to restrict the search to function names.
_functionSpecifierName :: String
_functionSpecifierName = "function"

-- Name for the specifier to restrict the search to type names.
_typeSpecifierName :: String
_typeSpecifierName = "type"

-- Name for the specifier to restrict the search to signatures.
_signatureSpecifierName :: String
_signatureSpecifierName = "signature"

-- Name for the specifier to search for information in the given module only.
_inModuleSpecifierName :: String
_inModuleSpecifierName = "inModule"

-- Name for the specifier to restrict the search to a module
--  coded by a given author.
_authorSpecifierName :: String
_authorSpecifierName = "author"

-- Name for the specifier to restrict the search to non-deterministic functions
_nondetSpecifierName :: String
_nondetSpecifierName = "nondet"

-- Name for the specifier to restrict the search to deterministic functions.
_detSpecifierName :: String
_detSpecifierName = "det"

-- Name for the specifier to restrict the search to flexible functions.
_flexibleSpecifierName :: String
_flexibleSpecifierName = "flexible"

-- Name for the specifier to restrict the search to rigid functions.
_rigidSpecifierName :: String
_rigidSpecifierName = "rigid"

_moduleSpecifierNameShort :: String
_moduleSpecifierNameShort = "m"

_functionSpecifierNameShort :: String
_functionSpecifierNameShort = "f"

_typeSpecifierNameShort :: String
_typeSpecifierNameShort = "t"

_signatureSpecifierNameShort :: String
_signatureSpecifierNameShort = "s"

_inModuleSpecifierNameShort :: String
_inModuleSpecifierNameShort = "in"

_authorSpecifierNameShort :: String
_authorSpecifierNameShort = "a"

_nondetSpecifierNameShort :: String
_nondetSpecifierNameShort = "nd"

_detSpecifierNameShort :: String
_detSpecifierNameShort = "d"

_flexibleSpecifierNameShort :: String
_flexibleSpecifierNameShort = "fl"

_rigidSpecifierNameShort :: String
_rigidSpecifierNameShort = "ri"

--------------------------
-- the signature parser --
--------------------------

-- | Parses type variables (i.e. a character) and following whitespaces due
--    to lexeme.
varParser :: TypeExprParser
varParser =
 (var . head) <$> many1 (lexemer lower)

-- | Parses primitive (unary) types (i.e. Int, Float, Bool ...) and following
--    whitespaces due to identifier that is provided by signatureParser.
primParser :: TypeExprParser
primParser =
  prim <$> sigIdentifier

-- | Parses a list.
listParser :: TypeExprParser
listParser =
  (\texpr -> cons "[]" [texpr]) <$> bracket (signatureParser True)

-- | Parses a tuple.
tupleParser :: TypeExprParser
tupleParser =
  try ((\_ -> cons "()" []) <$> aSymbol "()")
  <|> ((\tuple -> cons (tupleCons tuple) tuple)
       <$> paren parseTuple)
 where tupleCons list = "(" ++ replicate (length list - 1) ',' ++ ")"
       parseTuple = (\item _ itemList -> item:itemList) <$>
                     (signatureParser True)
                     <*> (aSymbol ",")
                     <*> sepBy1 (signatureParser True) (aSymbol ",")

-- | Parses a type constructor.
--   A whitespace is used as identicator, if it is followed by another
--    successful call of the signatureParser.
consParser :: TypeExprParser
consParser =
  (\constr _ expr -> cons constr expr)
   <$> sigIdentifier
   <*> whitespace
   <*> sepBy1 (signatureTerm False) whitespace

-- | All possible forms of signatures.
--   The first boolean value indicates, if a type constructor may appear without
--   parentheses, the second boolean value triggers special handling for
--   type variables.
signatureTerm :: Bool -> TypeExprParser
signatureTerm allowConsParser =
  (guard allowConsParser >> try consParser)
  <|> try tupleParser
  <|> paren (signatureParser False)
  <|> listParser
  <|> primParser
  <|> varParser


-- helper function to define a general way for handling binary operators
binary :: GenTokenParser s u m -> String -> (a -> a -> a)
          -> Assoc -> Operator s u m a
binary parser name fun =
  Infix ((\_ -> fun) <$> lexeme parser (reservedOp parser name))

-- Defines the binary "->"-operator used in the signature parser.
-- It has a right associativity and returns the partial application "FuncType".
signatureTable :: [[OperatorTable TypeExpr]]
signatureTable = [[binary signatureTokenParser "->" (-->) AssocRight]]

-- | Builds an expression parser for signatures with the given
--    term (signatureTerm) and table (signatureTable).
signatureParser :: Bool -> TypeExprParser
signatureParser _inAListOrTuple =
  buildExpressionParser signatureTable (signatureTerm True)

--------------------------
-- the specifier parser --
--------------------------

-- Helper function to build a parser that parses a given specifier name and
--  yields a query as specifier (i.e. Specifier [spec] identifier).
-- The boolean value indicates if a following identifier is allowed.
aSpecifierParser :: Bool
                 -> String
                 -> String
                 -> Parsec String String String
                 -> QueryParser
aSpecifierParser optionalIdent spec short parser =
  build [spec] (":"++spec)
  <|> build [spec] (":"++short)
 where build specList str =
        specify specList <$> (specReservedOp str *>
         if optionalIdent then option ("") parser
                          else parserReturn "")

-- | Parses a moduleSpecifierName followed by an identifier
--    (i.e. ":module Prelude").
moduleSpecifier :: QueryParser
moduleSpecifier =
  aSpecifierParser True
                   _moduleSpecifierName
                   _moduleSpecifierNameShort
                   specIdentifier

-- | Parses a functionSpecifierName followed by an identifier
--    (i.e. ":function map").
--   It can also parse operators and parenthesized operators (i.e. (+) or +).
functionSpecifier :: QueryParser
functionSpecifier =
  aSpecifierParser True _functionSpecifierName _functionSpecifierNameShort
   (specIdentifier
    <|> binOperator
    <|> try ((\a b c -> (a ++ b ++ c)) <$>
                 aSymbol "(" <*> many (char ',') <*> aSymbol ")")
    <|> paren binOperator
    <|> (aSymbol "[]"))

-- | Parses a typeSpecifierName followed by an identifier (i.e. ":type Either").
--   It can also parse ":" and "[]" as they are special constructors.
typeSpecifier :: QueryParser
typeSpecifier =
  aSpecifierParser True _typeSpecifierName  _typeSpecifierNameShort
   (specIdentifier <|> (aSymbol ":") <|> (aSymbol "[]"))

-- | Parses a inModuleSpecifierName followed by an identifier
--    (i.e. ":in Prelude").
inModuleSpecifier :: QueryParser
inModuleSpecifier =
  aSpecifierParser True
                   _inModuleSpecifierName
                   _inModuleSpecifierNameShort
                    specIdentifier

-- | Parses a authorSpecifierName followed by an identifier
--    (i.e. ":author frank").
authorSpecifier :: QueryParser
authorSpecifier =
  aSpecifierParser True
                   _authorSpecifierName
                   _authorSpecifierNameShort
                   specIdentifier

-- | Parses a nondetSpecifierName.
nondeterminismSpecifier :: QueryParser
nondeterminismSpecifier =
  aSpecifierParser False
                   _nondetSpecifierName
                   _nondetSpecifierNameShort
                   specIdentifier

-- | Parses a detSpecifierName.
determinismSpecifier :: QueryParser
determinismSpecifier =
  aSpecifierParser False _detSpecifierName _detSpecifierNameShort specIdentifier

-- | Parses a flexibleSpecifierName.
flexibleSpecifier :: QueryParser
flexibleSpecifier =
  aSpecifierParser False
                   _flexibleSpecifierName
                   _flexibleSpecifierNameShort
                   specIdentifier

-- | Parses a rigidSpecifierName.
rigidSpecifier :: QueryParser
rigidSpecifier =
  aSpecifierParser False
                   _rigidSpecifierName
                   _rigidSpecifierNameShort
                   specIdentifier

-- | Parses a signatureSpecifier followed by a valid signature
--    (i.e. ":signature Int->Int").
signatureSpecifier :: QueryParser
signatureSpecifier = signatureSpecifier' _signatureSpecifierName
                 <|> signatureSpecifier' _signatureSpecifierNameShort
 where
  signatureSpecifier' name =
   (\sig -> specify [_signatureSpecifierName] (testShow sig))
     <$> (specReservedOp (":"++name) *> signatureParser False)

-- | Parses all possible forms of specifiers.
specifierParser :: QueryParser
specifierParser =
  try $ paren specifierParser
  <|> moduleSpecifier
  <|> typeSpecifier
  <|> inModuleSpecifier
  <|> authorSpecifier
  <|> nondeterminismSpecifier
  <|> determinismSpecifier
  <|> flexibleSpecifier
  <|> rigidSpecifier
  <|> functionSpecifier
  <|> signatureSpecifier

----------------------
-- the query parser --
----------------------

-- | Parses all possible forms of a user query: a specifier or the whole query
--    with parentheses or permutation of specifiers, identifiers and signatures.
binOpTerm :: QueryParser
binOpTerm =
  try ((\a -> binQuery a) <$>
   many1 (specifierParser
         <|> try (Word <$> moduleIdentifier)
         <|> try (Word <$> binIdentifier)
         <|> try (Word <$> lexemer (aSymbol "[]"))
         <|> try ((\a b c -> Word (a ++ b ++ c)) <$>
                    aSymbol "(" <*> many (char ',') <*> aSymbol ")")
         <|> try (Word <$> binOperator)) <* eof)
  <|> try (binQuery <$> many1 ((\word -> specify [_signatureSpecifierName]
                                                 (testShow word))
                                 <$> signatureParser False))
  <|> try (paren binOpParser)
  <|> try (paren specifierParser)
  <|> ((\_ -> ' ') <$> aSymbol "(") *> binOpParser

-- | Defines the binary operators "AND", "OR", and "NOT".
--   The order in the table represents the precendens (first = highest).
--   When parsing a operator, it returns the partial application of "BinQuery"
--    (and the given operator).
binOpTable :: [[OperatorTable Query]]
binOpTable = [[binary binaryTokenParser "AND" (BinQuery And) AssocLeft],
              [binary binaryTokenParser "OR" (BinQuery Or) AssocLeft],
              [binary binaryTokenParser "NOT" (BinQuery But) AssocLeft]]

-- | Builds an expression parser for the given table
--    (binOpTable) and term (binOpTerm).
binOpParser :: QueryParser
binOpParser = buildExpressionParser binOpTable binOpTerm

-- | Top-level parser that is used to parse the query the user is searching for.
queryParser :: QueryParser
queryParser = whitespace *> binOpParser

-- | Runs the parser, the only exported function.
parse :: String -> Either String Query
parse = result . runP queryParser "" "curr(y)gle"
 where result (Left err) = Left (show err)
       result (Right q)  = Right q

--------------------------------------------------------------------------------
{- The expression makeTokenParser language creates a GenTokenParser record that
    contains lexical parsers that are defined using the definitions in the
    language record (i.e. identifier, which also fails on reserved names).
-}
--------------------------------------------------------------------------------

signatureDef :: LanguageDef String
signatureDef = emptyDef
  { identStart      = upper
  , identLetter     = alphaNum
  , reservedNames   = ["AND", "NOT", "OR"]
  }

signatureTokenParser :: TokenParser String
signatureTokenParser = makeTokenParser signatureDef

sigIdentifier :: StringParser
sigIdentifier = identifier signatureTokenParser

specifierDef :: LanguageDef String
specifierDef = emptyDef
  { identStart      = alphaNum
  , identLetter     = alphaNum
  , reservedNames   = ["AND", "NOT", "OR"]
  }

moduleDef :: LanguageDef String
moduleDef = emptyDef
  { identStart      = upper
  , identLetter     = alphaNum <|> oneOf "."
  , reservedNames   = ["AND", "NOT", "OR"]
  }


moduleTokenParser :: TokenParser String
moduleTokenParser = makeTokenParser moduleDef

moduleIdentifier :: StringParser
moduleIdentifier = identifier moduleTokenParser

specifierTokenParser :: TokenParser String
specifierTokenParser = makeTokenParser specifierDef

specIdentifier :: StringParser
specIdentifier = identifier specifierTokenParser

specReservedOp :: String -> Parser ()
specReservedOp = reservedOp specifierTokenParser

binOpDef :: LanguageDef String
binOpDef = emptyDef
  { identStart      = lower <|> oneOf "_"
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = opLetter binOpDef
  , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~_"
  , reservedOpNames = ["->"]
  , reservedNames   =
     ["AND", "NOT", "OR", ":signature", ":module", ":function",":type",
      ":nondet", ":det", ":flexible", ":rigid", ":in", "->"]
  }

binaryTokenParser :: TokenParser String
binaryTokenParser = makeTokenParser binOpDef

whitespace :: Parser ()
whitespace = whiteSpace binaryTokenParser

bracket :: Parser a -> Parser a
bracket = brackets binaryTokenParser

paren :: Parser a -> Parser a
paren = parens binaryTokenParser

lexemer :: Parser a -> Parser a
lexemer = lexeme binaryTokenParser

binOperator :: StringParser
binOperator = operator binaryTokenParser

binIdentifier :: StringParser
binIdentifier = do
  ident <- identifier binaryTokenParser
  guard (length ident >= 1) >> return ident

aSymbol :: String -> StringParser
aSymbol = symbol binaryTokenParser

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

type StringParser    = Parser String
type TypeExprParser  = Parser TypeExpr
type QueryParser     = Parser Query
type OperatorTable a = Operator String String Identity a

type Parser a = Parsec String String a

-- shortcut prettyprint
testShow :: TypeExpr -> String
testShow = showType "" False

-- Converts a list of queries one query by folding left.
-- Theoretical, non-empty lists cannot occur, because we use `binQuery`
--  on the resulting list parsed with `many1`.
binQuery :: [Query] -> Query
binQuery = foldl1 (BinQuery And)
