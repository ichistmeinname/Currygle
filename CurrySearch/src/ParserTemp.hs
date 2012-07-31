import Data.Char (ord)
import Text.Parsec.Expr (buildExpressionParser, Operator (..), Assoc (..))
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim  (runP, Parsec, try, many, (<?>))
import Text.Parsec.Perm
import Text.Parsec.Combinator (sepBy1, eof, many1, notFollowedBy)
import Text.Parsec.Char (upper, alphaNum, oneOf, noneOf, lower, anyChar)
import Control.Applicative ((<*>), (<$>), (<|>), (*>), (<*), empty)
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
  BinQuery And (Specifier [":signature"] (Word "(Int -> Int) -> Int)")) (specify [":type"] "b") ~=? unRight (run binaryParser "((Int->Int)->Int) AND (:type b)")

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
   <*> sepBy1 (tExprParser True) (whiteSpace signatureTokenParser)

tExprParser :: Bool -> Parsec String String TypeExpr
tExprParser paren = buildExpressionParser signatureTable (signatureTerm paren)

binOpParser :: Parsec String String Query
binOpParser = buildExpressionParser binOpTable binaryTerm

signatureParser :: Parsec String String TypeExpr
signatureParser =
--  whiteSpace signatureTokenParser *>  
  (tExprParser False)

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
  moduleSpecifier
  <|> typeSpecifier
  <|> functionSpecifier
  <|> signatureSpecifier

binaryParser :: Parsec String String Query
binaryParser = whiteSpace binaryTokenParser *> binOpParser <* eof

signatureTerm :: Bool -> Parsec String String TypeExpr
signatureTerm paren = 
  try (if paren then parens signatureTokenParser consParser else consParser)
  <|> try ((\tupel -> cons (tupelCons tupel) tupel) 
     <$> parens signatureTokenParser tupelParser)
  <|> listParser
  <|> parens signatureTokenParser (tExprParser False)         
  <|> primParser
  <|> varParser
  where tupelCons list = "(" ++ replicate (length list - 1) ',' ++ ")"

listParser :: Parsec String String TypeExpr
listParser = (\texpr -> cons "[]" [texpr]) <$> brackets signatureTokenParser (tExprParser False)

tupelParser :: Parsec String String [TypeExpr]
tupelParser = (\item _ itemList -> item:itemList) <$> (tExprParser False) <*> symbol signatureTokenParser "," <*> sepBy1 (tExprParser False) (symbol signatureTokenParser ",")

binaryTerm :: Parsec String String Query
binaryTerm =
  -- try $ parens binaryTokenParser binOpParser
  -- <|> try (specifierParser <* eof)
  -- <|> 
  try (Word <$> identifier binaryTokenParser)
  <|> permutations
  <|> parens binaryTokenParser binOpParser 

permutations = 
  try $ permute ((\specs1 identsig specs2-> binQuery (specs1++identsig++specs2)) 
   <$$> many specifierParser
        <||> many (try (Word <$> (identifier binaryTokenParser <* notFollowedBy (oneOf "-> ")))
             <|> try ((\word -> specify [":signature"] (testShow word)) 
                         <$> signatureParser)
             <|> Word <$> (identifier binaryTokenParser <?> "lastHelp"))
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

-- instance Show TExpr where
--   -- show (PrimType str)         = str
--   show (FuncType expr1 expr2) = "(" ++ show expr1 ++ " -> " ++ show expr2 ++ ")"
--   show (TCons (_,name) [])        = name
--   show (TCons (_,name) exprList)  = "(" ++ name ++ " " ++ intercalate " " (map show exprList) ++ ")"
--   show (TVar name)            = name

-- data TExpr = FuncType TExpr TExpr | --PrimType String | 
--              TCons (String,String) [TExpr] | TVar String
--   deriving (Eq)

-- -- | The query language.
-- data Query = Word       String            -- ^ Single case-insensitive word.
--            | Phrase     String            -- ^ Single case-insensitive phrase.
--            | CaseWord   String            -- ^ Single case-sensitive word.
--            | CasePhrase String            -- ^ Single case-sensitive phrase.
--            | FuzzyWord  String            -- ^ Single fuzzy word.
--            | Specifier  [Context] Query   -- ^ Restrict query to a list of contexts.
--            | Negation   Query             -- ^ Negate the query.
--            | BinQuery   BinOp Query Query -- ^ Combine two queries through a binary operation.
--            deriving (Eq, Show)

-- type Context = String

-- -- | A binary operation.
-- data BinOp = And  -- ^ Intersect two queries.
--            | Or   -- ^ Union two queries.
--            | But  -- ^ Filter a query by another, @q1 BUT q2@ is equivalent to @q1 AND NOT q2@.
--            deriving (Eq, Show)

testShow = showType "" False

-- showType :: String -> Bool -> TExpr -> String
-- showType _ _ (TVar str) = str
-- showType modName nested (FuncType t1 t2) =
--    parenthesis nested
--     (showType modName (isFunctionType t1) t1 ++ " -> " ++ showType modName False t2)
-- showType modName nested (TCons tc ts)
--  | null ts = showTypeCons modName tc
--  | tc==("Prelude","[]") && (isString $ head ts) = 
--      "String"
--  | tc==("Prelude","[]") =
--      "[" ++ showType modName False (head ts) ++ "]" -- list type
--  | take 2 (snd tc) == "(," =                        -- tuple type
--      "(" ++ intercalate "," (map (showType modName False) ts) ++ ")"
--  | otherwise = 
--      parenthesis nested
--       (showTypeCons modName tc ++ " " ++
--        intercalate " " (map (showType modName True) ts))
-- -- showType _ _ _ = ""

-- parenthesis :: Bool -> String -> String
-- parenthesis p str 
--     | p         = "(" ++ str ++ ")"
--     | otherwise = str 

-- isString :: TExpr -> Bool
-- isString (TCons ("Prelude","Char") []) = True
-- isString _                             = False

-- isFunctionType :: TExpr -> Bool
-- isFunctionType (FuncType _ _) = True
-- isFunctionType _              = False

-- isQualifiedName :: String -> String -> Bool
-- isQualifiedName moduleName fModuleName = moduleName == fModuleName || fModuleName == "Prelude"

-- qualifiedName :: String -> String -> String -> String
-- qualifiedName moduleName fModuleName funcName = 
--   if isQualifiedName moduleName fModuleName then funcName else moduleName ++ "." ++ funcName

-- showTypeCons :: String -> (String, String) -> String
-- showTypeCons modName (mtc,tc) =
--   qualifiedName modName mtc tc