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
import Data.Functor.Identity (Identity)

import Control.Applicative ((<*>), (<$>), (<|>), (*>), (<*))

import Text.Parsec.Token
import Text.Parsec.Prim  (runP, Parsec, try, many, parserZero, parserReturn)
import Text.Parsec.Perm
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr (buildExpressionParser, Operator (..), Assoc (..))
import Text.Parsec.Error (ParseError)
import Text.Parsec.Combinator (sepBy1, eof, many1, notFollowedBy, option)
import Text.Parsec.Char (upper, alphaNum, oneOf, lower)
import Test.HUnit

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


---------------------------------------------------------------------------------------
----------------------------------- A LOT OF TESTS ------------------------------------
---------------------------------------------------------------------------------------

runAllTests :: IO Counts
runAllTests = runSigTests >> runBinTests >> runSpecTests >> runParenTests

runSigTests :: IO Counts
runSigTests = runTestTT sigTests

sigTests :: Test
sigTests = test [simpleFuncType, funcTypeWithWhitespace, funcTypeWithParens, manyWhitespaces, leadingWhitespaces, typeConstructor, naryTypeConstructor, 
              consParserTest, primWithParens, moreParens, varParserTest, consParser2Test, tupelTest, listTest, listTupelTest]

simpleFuncType = 
  unRight (run queryParser "Int->Int") ~=? (Specifier [":signature"] (Word $ testShow (prim "Int" --> prim "Int")))
funcTypeWithWhitespace = 
  unRight (run queryParser "String -> String") ~=? (Specifier [":signature"] (Word  $ testShow (prim "String" --> prim "String")))
funcTypeWithParens = 
  unRight (run queryParser "(String -> String) -> Float") ~=? (Specifier [":signature"](Word $ testShow ((prim "String" --> prim "String") --> prim "Float")))
manyWhitespaces = 
  unRight (run queryParser "String         ->     String    ") ~=? (Specifier [":signature"](Word $ testShow (prim "String" --> prim "String")))
leadingWhitespaces = 
  unRight (run queryParser "  String ") ~=? (Specifier [":signature"](Word $ testShow (prim "String")))
typeConstructor =
  unRight (run queryParser "IO String") ~=? (Specifier [":signature"](Word $ testShow (cons "IO" [prim "String"])))
naryTypeConstructor = 
  unRight (run queryParser "Something Int Float Int Float (Int->Int)") ~=? (Specifier [":signature"](Word $ testShow (cons "Something" (map prim ["Int", "Float", "Int", "Float"] ++ [prim "Int" --> prim "Int"]))))
consParserTest =
  unRight (run queryParser "Something Int Float") ~=? (Specifier [":signature"](Word $ testShow (cons "Something" [prim "Int", prim "Float"])))
primWithParens =
  unRight (run queryParser "(A)") ~=? (Specifier [":signature"](Word $ testShow (prim "A")))
moreParens =
  unRight (run queryParser "( ( B  ) ) ") ~=? (Specifier [":signature"](Word $ testShow (prim "B")))
varParserTest =
  unRight (run queryParser "Maybe a") ~=? (Specifier [":signature"](Word $ testShow (cons "Maybe" [var "a"])))
consParser2Test =
  unRight (run queryParser "String -> Maybe Int") ~=? (Specifier [":signature"](Word $ testShow (prim "String" --> cons "Maybe" [prim "Int"])))
tupelTest =
  (Specifier [":signature"] $ Word (testShow (TCons ("","(,,)") [prim "Int", prim "Float" --> prim "Map", prim "Int" --> prim "Float" --> prim "Map"]))) ~=? unRight (run queryParser "(Int, Float -> Map, Int -> Float -> Map)")
listTest = 
  (Specifier [":signature"] $ Word (testShow (TCons ("","[]") [prim "Float" --> prim "Map"]))) ~=? unRight (run queryParser "[Float -> Map]")
listTupelTest = 
 (Specifier [":signature"] $ Word (testShow (TCons ("","(,)") [prim "Float" --> prim "Int", TCons ("","[]") [prim "Map"]]))) ~=? unRight (run queryParser "(Float -> Int, [Map])")

runBinTests :: IO Counts
runBinTests = runTestTT binTests

binTests :: Test
binTests = test [andTest, orTest, notTest, multipleBins, nestedBinsAndSigs]

andTest =
  BinQuery And (Word "a") (Word "b") ~=? unRight (run queryParser "a AND b")
orTest =
  BinQuery Or (Word "a") (Word "b") ~=? unRight (run queryParser "a OR b")
notTest =
  BinQuery But (Word "a") (Word "b") ~=? unRight (run queryParser "a NOT b")
multipleBins =
  BinQuery But (BinQuery Or (BinQuery Or (BinQuery And (Word "a") (Word "b")) (Word "c")) (Word "d")) (Word "e") ~=? unRight (run queryParser "a AND b OR c OR d NOT e")
nestedBinsAndSigs =
  BinQuery And ((Specifier [":signature"] $ Word $ testShow (prim "Int" --> prim "Int"))) ((Specifier [":signature"] $ Word "Float")) ~=? unRight (run queryParser "Int -> Int AND Float")

runSpecTests = runTestTT specTests

specTests :: Test
specTests = test [signatureTest, allAtOnce, andAllAtOnce]

signatureTest =
  Specifier [":signature"] (Word $ testShow (prim "Int" --> prim "String")) ~=? unRight (run queryParser ":signature Int->String")
allAtOnce = 
  BinQuery And (BinQuery And (BinQuery And (Specifier [":module"] (Word "Prelude")) (Specifier [":function"] (Word "map"))) (Specifier [":signature"] (Word "Int -> Int"))) (Specifier [":type"] (Word "something")) ~=? unRight (run queryParser ":type something Int->Int :module Prelude :function map")
andAllAtOnce =
  BinQuery And (BinQuery And (BinQuery And (Specifier [":type"] (Word "something")) (Specifier [":signature"] (Word "Int -> Int"))) (Specifier [":module"] (Word "Prelude"))) (Specifier [":function"] (Word "map")) ~=? unRight (run queryParser ":type something AND Int->Int AND :module Prelude AND :function map")

runParenTests :: IO Counts
runParenTests = runTestTT parenTests

parenTests :: Test
parenTests = test [p1, p2, p3, p4]

p1 = 
  BinQuery And (Specifier [":signature"] (Word "(Int -> Int) -> Int")) (specify [":type"] "b") ~=? unRight (run queryParser "((Int->Int)->Int) AND (:type b)")

p2 = 
  BinQuery And (Specifier [":signature"] (Word "Int -> Int")) (specify [":type"] "b") ~=? unRight (run queryParser "(Int -> Int) AND (:type b)")

p3 = 
  BinQuery And (Specifier [":signature"] (Word "Int -> Int")) (specify [":type"] "b") ~=? unRight (run queryParser "(Int -> Int AND :type b)")

p4 = 
  BinQuery And (Specifier [":signature"] (Word "Int -> Int")) (specify [":type"] "b") ~=? unRight (run queryParser "Int -> Int AND :type b")

unRight :: Either a b -> b
unRight (Right b) = b

run p = runP p "" ""

--------------------------
-- the signature parser --
--------------------------

-- | Parses type varibales and following whitespaces due to lexeme.
varParser :: TypeExprParser
varParser = var <$> lexeme signatureTokenParser (many1 lower)

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
signatureTable :: [[OperatorTable TypeExpr]]
signatureTable = [[binary signatureTokenParser "->" FuncType AssocRight]]

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
       result (Right q)  = Right q

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

var :: String -> TypeExpr
var str = TVar ((ord $ head str) - 97)

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
type OperatorTable a = Operator String String Identity a

-- shortcut prettyprint
testShow :: TypeExpr -> String
testShow = showType "" False

-- Converts a list of queries one query by folding left.
binQuery :: [Query] -> Query
binQuery [] = Word ""
binQuery (q:qs) = foldl (BinQuery And) q qs
