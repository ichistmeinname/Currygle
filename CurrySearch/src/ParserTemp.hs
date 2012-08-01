import Data.Char (ord)
import Text.Parsec.Expr (buildExpressionParser, Operator (..), Assoc (..))
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim  (runP, Parsec, try, many, (<?>))
import Text.Parsec.Perm
import Text.Parsec.Combinator (sepBy1, eof, many1, notFollowedBy, option, optional, anyToken)
import Text.Parsec.Char (upper, alphaNum, oneOf, noneOf, lower, anyChar)
import Control.Applicative ((<*>), (<$>), (<|>), (*>), (<*))
import Test.HUnit

import Holumbus.Query.Language.Grammar

import CurryInfo
import Helpers (showType)

infixr 4 -->

runAllTests :: IO Counts
runAllTests = runSigTests >> runBinTests >> runSpecTests >> runParenTests

runSigTests :: IO Counts
runSigTests = runTestTT sigTests

sigTests :: Test
sigTests = test [simpleFuncType, funcTypeWithWhitespace, funcTypeWithParens, manyWhitespaces, leadingWhitespaces, typeConstructor, naryTypeConstructor, 
              consParserTest, primWithParens, moreParens, varParserTest, consParser2Test, tupelTest, listTest, listTupelTest]

simpleFuncType = 
  unRight (run binaryParser "Int->Int") ~=? (Specifier [":signature"] (Word $ testShow (prim "Int" --> prim "Int")))
funcTypeWithWhitespace = 
  unRight (run binaryParser "String -> String") ~=? (Specifier [":signature"] (Word  $ testShow (prim "String" --> prim "String")))
funcTypeWithParens = 
  unRight (run binaryParser "(String -> String) -> Float") ~=? (Specifier [":signature"](Word $ testShow ((prim "String" --> prim "String") --> prim "Float")))
manyWhitespaces = 
  unRight (run binaryParser "String         ->     String    ") ~=? (Specifier [":signature"](Word $ testShow (prim "String" --> prim "String")))
leadingWhitespaces = 
  unRight (run binaryParser "  String ") ~=? (Specifier [":signature"](Word $ testShow (prim "String")))
typeConstructor =
  unRight (run binaryParser "IO String") ~=? (Specifier [":signature"](Word $ testShow (cons "IO" [prim "String"])))
naryTypeConstructor = 
  unRight (run binaryParser "Something Int Float Int Float (Int->Int)") ~=? (Specifier [":signature"](Word $ testShow (cons "Something" (map prim ["Int", "Float", "Int", "Float"] ++ [prim "Int" --> prim "Int"]))))
consParserTest =
  unRight (run binaryParser "Something Int Float") ~=? (Specifier [":signature"](Word $ testShow (cons "Something" [prim "Int", prim "Float"])))
primWithParens =
  unRight (run binaryParser "(A)") ~=? (Specifier [":signature"](Word $ testShow (prim "A")))
moreParens =
  unRight (run binaryParser "( ( B  ) ) ") ~=? (Specifier [":signature"](Word $ testShow (prim "B")))
varParserTest =
  unRight (run binaryParser "Maybe a") ~=? (Specifier [":signature"](Word $ testShow (cons "Maybe" [var "a"])))
consParser2Test =
  unRight (run binaryParser "String -> Maybe Int") ~=? (Specifier [":signature"](Word $ testShow (prim "String" --> cons "Maybe" [prim "Int"])))
tupelTest =
  (Specifier [":signature"] $ Word (testShow (TCons ("","(,,)") [prim "Int", prim "Float" --> prim "Map", prim "Int" --> prim "Float" --> prim "Map"]))) ~=? unRight (run binaryParser "(Int, Float -> Map, Int -> Float -> Map)")
listTest = 
  (Specifier [":signature"] $ Word (testShow (TCons ("","[]") [prim "Float" --> prim "Map"]))) ~=? unRight (run binaryParser "[Float -> Map]")
listTupelTest = 
 (Specifier [":signature"] $ Word (testShow (TCons ("","(,)") [prim "Float" --> prim "Int", TCons ("","[]") [prim "Map"]]))) ~=? unRight (run binaryParser "(Float -> Int, [Map])")

runBinTests :: IO Counts
runBinTests = runTestTT binTests

binTests :: Test
binTests = test [andTest, orTest, notTest, multipleBins, nestedBinsAndSigs]

andTest =
  BinQuery And (Word "a") (Word "b") ~=? unRight (run binaryParser "a AND b")
orTest =
  BinQuery Or (Word "a") (Word "b") ~=? unRight (run binaryParser "a OR b")
notTest =
  BinQuery But (Word "a") (Word "b") ~=? unRight (run binaryParser "a NOT b")
multipleBins =
  BinQuery But (BinQuery Or (BinQuery Or (BinQuery And (Word "a") (Word "b")) (Word "c")) (Word "d")) (Word "e") ~=? unRight (run binaryParser "a AND b OR c OR d NOT e")
nestedBinsAndSigs =
  BinQuery And ((Specifier [":signature"] $ Word $ testShow (prim "Int" --> prim "Int"))) ((Specifier [":signature"] $ Word "Float")) ~=? unRight (run binaryParser "Int -> Int AND Float")

runSpecTests = runTestTT specTests

specTests :: Test
specTests = test [signatureTest, allAtOnce, andAllAtOnce]

signatureTest =
  Specifier [":signature"] (Word $ testShow (prim "Int" --> prim "String")) ~=? unRight (run binaryParser ":signature Int->String")
allAtOnce = 
  BinQuery And (BinQuery And (BinQuery And (Specifier [":module"] (Word "Prelude")) (Specifier [":function"] (Word "map"))) (Specifier [":signature"] (Word "Int -> Int"))) (Specifier [":type"] (Word "something")) ~=? unRight (run binaryParser ":type something Int->Int :module Prelude :function map")
andAllAtOnce =
  BinQuery And (BinQuery And (BinQuery And (Specifier [":type"] (Word "something")) (Specifier [":signature"] (Word "Int -> Int"))) (Specifier [":module"] (Word "Prelude"))) (Specifier [":function"] (Word "map")) ~=? unRight (run binaryParser ":type something AND Int->Int AND :module Prelude AND :function map")

runParenTests :: IO Counts
runParenTests = runTestTT parenTests

parenTests :: Test
parenTests = test [p1, p2, p3, p4]

p1 = 
  BinQuery And (Specifier [":signature"] (Word "(Int -> Int) -> Int")) (specify [":type"] "b") ~=? unRight (run binaryParser "((Int->Int)->Int) AND (:type b)")

p2 = 
  BinQuery And (Specifier [":signature"] (Word "Int -> Int")) (specify [":type"] "b") ~=? unRight (run binaryParser "(Int -> Int) AND (:type b)")

p3 = 
  BinQuery And (Specifier [":signature"] (Word "Int -> Int")) (specify [":type"] "b") ~=? unRight (run binaryParser "(Int -> Int AND :type b)")

p4 = 
  BinQuery And (Specifier [":signature"] (Word "Int -> Int")) (specify [":type"] "b") ~=? unRight (run binaryParser "Int -> Int AND :type b")

unRight :: Either a b -> b
unRight (Right b) = b

run p = runP p "" ""

binary parser name fun = 
  Infix ((\_ -> fun) <$> lexeme parser 
                                (reservedOp parser name))

signatureTable = [[binary signatureTokenParser "->" FuncType AssocRight]]

binOpTable = [[binary binaryTokenParser "AND" (BinQuery And) AssocLeft], 
              [binary binaryTokenParser "OR" (BinQuery Or) AssocLeft], 
              [binary binaryTokenParser "NOT" (BinQuery But) AssocLeft]]

varParser :: Parsec String String TypeExpr
varParser = var <$> lexeme signatureTokenParser (many1 lower)

primParser :: Parsec String String TypeExpr
primParser = prim <$> identifier signatureTokenParser

consParser :: Parsec String String TypeExpr
consParser = 
  (\constr _ expr -> cons constr expr) 
   <$> identifier signatureTokenParser 
   <*> whiteSpace signatureTokenParser 
   <*> sepBy1 temp (whiteSpace signatureTokenParser)

temp = 
  try tupelParser 
  <|> parens signatureTokenParser (tExprParser False) 
  <|> unaryTExpr

tExprParser :: Bool -> Parsec String String TypeExpr
tExprParser paren = buildExpressionParser signatureTable 
                    (signatureTerm paren)

unaryTExpr = 
  -- try ((\tupel -> cons (tupelCons tupel) tupel) 
     -- <$> parens signatureTokenParser tupelParser)
  listParser
  -- <|> parens signatureTokenParser (tExprParser False)         
  <|> primParser
  <|> varParser
  -- where tupelCons list = "(" ++ replicate (length list - 1) ',' ++ ")"

signatureTerm :: Bool -> Parsec String String TypeExpr
signatureTerm paren = 
  try consParser <|> temp
  -- <|> tupelParser
  -- <|> listParser
  -- <|> parens signatureTokenParser (tExprParser False)  
  -- <|> unaryTExpr       
  -- <|> primParser
  -- <|> varParser
  -- where tupelCons list = "(" ++ replicate (length list - 1) ',' ++ ")"

tupelParser =
  try ((\tupel -> cons (tupelCons tupel) tupel) 
       <$> parens signatureTokenParser parseTupel)
  where tupelCons list = "(" ++ replicate (length list - 1) ',' ++ ")"
        parseTupel = (\item _ itemList -> item:itemList) <$> 
                     (tExprParser False) <*> symbol signatureTokenParser "," 
                     <*> sepBy1 (tExprParser False) (symbol signatureTokenParser ",")
listParser :: Parsec String String TypeExpr
listParser = (\texpr -> cons "[]" [texpr]) <$> brackets signatureTokenParser (tExprParser False)

-- tupelParser :: Parsec String String [TypeExpr]
-- tupelParser = (\item _ itemList -> item:itemList) <$> (tExprParser False) <*> symbol signatureTokenParser "," <*> sepBy1 (tExprParser False) (symbol signatureTokenParser ",")

signatureParser :: Parsec String String TypeExpr
signatureParser = (tExprParser False)

binOpParser :: Parsec String String Query
binOpParser = buildExpressionParser binOpTable binaryTerm

aSpecifierParser :: String -> Parsec String String Query
aSpecifierParser str = 
  specify [str] <$> (reservedOp specifierTokenParser str *> identifier specifierTokenParser)

moduleSpecifier :: Parsec String String Query
moduleSpecifier = aSpecifierParser ":module"

functionSpecifier :: Parsec String String Query
functionSpecifier = aSpecifierParser ":function"

typeSpecifier :: Parsec String String Query
typeSpecifier = aSpecifierParser ":type"

signatureSpecifier :: Parsec String String Query
signatureSpecifier = 
 (\sig -> specify [":signature"] (testShow sig)) <$> 
  (reservedOp specifierTokenParser ":signature" *> signatureParser)

specifierParser :: Parsec String String Query
specifierParser =
  try $ parens specifierTokenParser specifierParser
  <|> moduleSpecifier
  <|> typeSpecifier
  <|> functionSpecifier
  <|> signatureSpecifier:

binaryParser :: Parsec String String Query
binaryParser = whiteSpace binaryTokenParser *> binOpParser <* eof

binaryTerm :: Parsec String String Query
binaryTerm =
  try (permutations *> parens specifierTokenParser specifierParser)
  <|> try (permutations *> parens binaryTokenParser binOpParser)
  <|> permutations

permutations :: Parsec String String Query
permutations = 
  try $ permute ((\specs1 identsig specs2-> binQuery (specs1++identsig++specs2)) 
   <$$> many specifierParser
        <||>  many (try (Word <$> (identifier binaryTokenParser <* notFollowedBy (oneOf "-> ")))
             <|> try ((\word -> specify [":signature"] (testShow word)) 
                         <$> signatureParser)
             <|> Word <$> (identifier binaryTokenParser))
        <||>  many specifierParser)

binQuery :: [Query] -> Query
binQuery [] = Word ""
binQuery (q:qs) = foldl (BinQuery And) q qs

signatureDef :: LanguageDef String
signatureDef = emptyDef 
               { identStart = upper,
                 identLetter = alphaNum,
                 opStart = oneOf "-",
                 opLetter = oneOf "->",
                 reservedOpNames = ["->"],
                 reservedNames = ["AND", "NOT", "OR",
                                  ":signature", ":module", ":function",":type", "->"]
               }

specifierDef :: LanguageDef String
specifierDef = emptyDef
               { identStart  = alphaNum,
                 identLetter = alphaNum,
                 opStart     = oneOf ":",
                 opLetter    = oneOf "signaturemodleypfc",
                 reservedOpNames = [":signature", ":module", ":function",":type"],
                 reservedNames   = ["AND","NOT","OR"]
               }

binOpDef :: LanguageDef String
binOpDef = emptyDef 
           { identStart = lower,
             identLetter = alphaNum,
             opStart = oneOf "AON",
             opLetter = oneOf "ANDORT",
             reservedOpNames = ["AND", "OR", "NOT"],
             reservedNames = ["->", ":signature", ":module", ":function",":type"]
           }

signatureTokenParser :: TokenParser String
signatureTokenParser = makeTokenParser signatureDef

specifierTokenParser :: TokenParser String
specifierTokenParser = makeTokenParser specifierDef

binaryTokenParser :: TokenParser String
binaryTokenParser = makeTokenParser binOpDef

-- some shortcut constructors
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

-- shortcut prettyprint
testShow :: TypeExpr -> String
testShow = showType "" False
