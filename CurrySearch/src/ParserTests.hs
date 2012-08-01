module ParserTests where

import Data.Char (ord)
import CurryInfo (TypeExpr (..))
import Helpers (showType)
import Test.HUnit
import Holumbus.Query.Language.Grammar
import Parser

infixr 4 -->

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
  unRight (parse "Int->Int") ~=? (Specifier [":signature"] (Word $ testShow (prim "Int" --> prim "Int")))
funcTypeWithWhitespace = 
  unRight (parse "String -> String") ~=? (Specifier [":signature"] (Word  $ testShow (prim "String" --> prim "String")))
funcTypeWithParens = 
  unRight (parse "(String -> String) -> Float") ~=? (Specifier [":signature"](Word $ testShow ((prim "String" --> prim "String") --> prim "Float")))
manyWhitespaces = 
  unRight (parse "String         ->     String    ") ~=? (Specifier [":signature"](Word $ testShow (prim "String" --> prim "String")))
leadingWhitespaces = 
  unRight (parse "  String ") ~=? (Specifier [":signature"](Word $ testShow (prim "String")))
typeConstructor =
  unRight (parse "IO String") ~=? (Specifier [":signature"](Word $ testShow (cons "IO" [prim "String"])))
naryTypeConstructor = 
  unRight (parse "Something Int Float Int Float (Int->Int)") ~=? (Specifier [":signature"](Word $ testShow (cons "Something" (map prim ["Int", "Float", "Int", "Float"] ++ [prim "Int" --> prim "Int"]))))
consParserTest =
  unRight (parse "Something Int Float") ~=? (Specifier [":signature"](Word $ testShow (cons "Something" [prim "Int", prim "Float"])))
primWithParens =
  unRight (parse "(A)") ~=? (Specifier [":signature"](Word $ testShow (prim "A")))
moreParens =
  unRight (parse "( ( B  ) ) ") ~=? (Specifier [":signature"](Word $ testShow (prim "B")))
varParserTest =
  unRight (parse "Maybe a") ~=? (Specifier [":signature"](Word $ testShow (cons "Maybe" [var 'a'])))
consParser2Test =
  unRight (parse "String -> Maybe Int") ~=? (Specifier [":signature"](Word $ testShow (prim "String" --> cons "Maybe" [prim "Int"])))
tupelTest =
  (Specifier [":signature"] $ Word (testShow (TCons ("","(,,)") [prim "Int", prim "Float" --> prim "Map", prim "Int" --> prim "Float" --> prim "Map"]))) ~=? unRight (parse "(Int, Float -> Map, Int -> Float -> Map)")
listTest = 
  (Specifier [":signature"] $ Word (testShow (TCons ("","[]") [prim "Float" --> prim "Map"]))) ~=? unRight (parse "[Float -> Map]")
listTupelTest = 
 (Specifier [":signature"] $ Word (testShow (TCons ("","(,)") [prim "Float" --> prim "Int", TCons ("","[]") [prim "Map"]]))) ~=? unRight (parse "(Float -> Int, [Map])")

runBinTests :: IO Counts
runBinTests = runTestTT binTests

binTests :: Test
binTests = test [andTest, orTest, notTest, multipleBins, nestedBinsAndSigs]

andTest =
  BinQuery And (Word "a") (Word "b") ~=? unRight (parse "a AND b")
orTest =
  BinQuery Or (Word "a") (Word "b") ~=? unRight (parse "a OR b")
notTest =
  BinQuery But (Word "a") (Word "b") ~=? unRight (parse "a NOT b")
multipleBins =
  BinQuery But (BinQuery Or (BinQuery Or (BinQuery And (Word "a") (Word "b")) (Word "c")) (Word "d")) (Word "e") ~=? unRight (parse "a AND b OR c OR d NOT e")
nestedBinsAndSigs =
  BinQuery And ((Specifier [":signature"] $ Word $ testShow (prim "Int" --> prim "Int"))) ((Specifier [":signature"] $ Word "Float")) ~=? unRight (parse "Int -> Int AND Float")

runSpecTests = runTestTT specTests

specTests :: Test
specTests = test [signatureTest, allAtOnce, andAllAtOnce]

signatureTest =
  Specifier [":signature"] (Word $ testShow (prim "Int" --> prim "String")) ~=? unRight (parse ":signature Int->String")
allAtOnce = 
  BinQuery And (BinQuery And (BinQuery And (Specifier [":module"] (Word "Prelude")) (Specifier [":function"] (Word "map"))) (Specifier [":signature"] (Word "Int -> Int"))) (Specifier [":type"] (Word "something")) ~=? unRight (parse ":type something Int->Int :module Prelude :function map")
andAllAtOnce =
  BinQuery And (BinQuery And (BinQuery And (Specifier [":type"] (Word "something")) (Specifier [":signature"] (Word "Int -> Int"))) (Specifier [":module"] (Word "Prelude"))) (Specifier [":function"] (Word "map")) ~=? unRight (parse ":type something AND Int->Int AND :module Prelude AND :function map")

runParenTests :: IO Counts
runParenTests = runTestTT parenTests

parenTests :: Test
parenTests = test [p1, p2, p3, p4]

p1 = 
  BinQuery And (Specifier [":signature"] (Word "(Int -> Int) -> Int")) (specify [":type"] "b") ~=? unRight (parse "((Int->Int)->Int) AND (:type b)")

p2 = 
  BinQuery And (Specifier [":signature"] (Word "Int -> Int")) (specify [":type"] "b") ~=? unRight (parse "(Int -> Int) AND (:type b)")

p3 = 
  BinQuery And (Specifier [":signature"] (Word "Int -> Int")) (specify [":type"] "b") ~=? unRight (parse "(Int -> Int AND :type b)")

p4 = 
  BinQuery And (Specifier [":signature"] (Word "Int -> Int")) (specify [":type"] "b") ~=? unRight (parse "Int -> Int AND :type b")

unRight :: Either a b -> b
unRight (Right b) = b

----------------------

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

testShow = showType "" False