----------------------------------------------------------------------
--- Functions to generate documentation in "CDoc" format.
---
--- @author Sandra Dylus
----------------------------------------------------------------------

module CurryDocCDoc where

import CurryDocParams
import CurryDocRead
import FlatCurry
import FlexRigid
-- import Char
-- import AnaCompleteness
import ReadShowTerm
import List



-- generateCDoc :: String  -> String -> [(SourceLine,String)] -> AnaInfo -> [String] -> IO (String, (String, [(String,String)]), [(SourceLine, (String, [(String,String)]))], [TypeDecl], [FuncDecl], [OpDecl], AnaInfo, [String])
-- generateCDoc :: String  -> String -> [(SourceLine,String)] -> AnaInfo -> IO ([String], (String, (String, [(String,String)]), [(SourceLine, (String, [(String,String)]))], [TypeDecl], [FuncDecl], [OpDecl]))
generateCDoc :: String  -> String -> [(SourceLine,String)] -> AnaInfo -> IO String
generateCDoc progName modCmts progCmts anaInfo = do
    putStrLn $ "Reading FlatCurry program \""++fcyName++"\"..."
    Prog modName imports types functions ops <- readFlatCurryFile fcyName
    let funcInfos = map funcInfo functions
        funcInfo (Func (mName, fName) _ _ tExpr (Rule _ expr)) = 
    	      		     	   (fName
    				   , typeSignature tExpr
    				   , mName
    				   , getFuncComment fName progCmts
    				   , getOverlappingInfo anaInfo (modName, fName)
    				   , getFlexRigid expr)
        modInfo (mCmts, avCmts) = (modName
    			     	   , versionOrAuthor "version" avCmts
    				   , versionOrAuthor "author" avCmts
    				   , imports
    				   , mCmts)
	modInfos = modInfo $ splitComment modCmts
    return $ showTerm (modInfos, funcInfos)
  where fcyName  = flatCurryFileName progName

-- auxilieres --------------------------------------------------------

versionOrAuthor :: String -> [(String, String)] -> String
versionOrAuthor string av = concat $ getCommentType string av


-- for special type constructors like [Int] or (String, String)
prettyPrintSpecialType :: String -> [TypeExpr] -> String
prettyPrintSpecialType cName tExprList  = 
    if null tExprList then cName else typeConstructorApplication cName tExprList

-- generates type signature 
typeSignature :: TypeExpr -> String
typeSignature (TCons (_, cName) tExprList)   	  	    = prettyPrintSpecialType cName tExprList
typeSignature (FuncType (TCons (_, cName) tExprList) tExpr) = prettyPrintSpecialType cName tExprList ++ ("->" ++ (typeSignature tExpr))

typeConstructorApplication :: String -> [TypeExpr] -> String
typeConstructorApplication cName tExprList =
    if isListConstructor cName then "[" ++ concatMap typeSignature tExprList ++ "]" 
       			       else "(" ++ intercalate "," (map typeSignature tExprList) ++ ")" 

isListConstructor :: String -> Bool
isListConstructor = ("[]" ==)

intercalate sep = concat . intersperse sep 