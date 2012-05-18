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
	typeInfos = map typeInfo types
	typeInfo (Type (mName, tName) _ _ consDecl) = 
		       	       	   (tName
	       	       	      	   , map consSignature consDecl	
				   , mName
				   , getDataComment tName progCmts)
        typeInfo (TypeSyn (mName, tName) _ _ tExpr) =
		 	  	  (tName
				  , [typeSignature tExpr]
				  , mName
				  , getDataComment tName progCmts)
    return $ showTerm (modInfos, funcInfos, typeInfos)
  where fcyName  = flatCurryFileName progName

-- auxilieres --------------------------------------------------------


consSignature :: ConsDecl -> [String]
consSignature (Cons (_, cName) _ _ tExprList) = cName : concatMap typeSignature tExprList

versionOrAuthor :: String -> [(String, String)] -> String
versionOrAuthor string av = concat $ getCommentType string av

-- for special type constructors like [Int] or (String, String)
prettyPrintType :: [TypeExpr] -> String
prettyPrintType [] = []	      -- shouldn't occur
prettyPrintType [tExpr] = "[" ++ concat (typeSignature tExpr) ++ "]"
prettyPrintType tExprList@(_:_:_) = "(" ++ intercalate "," (concatMap typeSignature tExprList) ++ ")"

-- generate type signature 
typeSignature :: TypeExpr -> [String]
typeSignature (TCons (_, cName) tExprList) = 
   if null tExprList then cName : [] else [prettyPrintType tExprList]
typeSignature (FuncType (TCons (_, cName) []) tExpr) =
  cName : typeSignature tExpr
typeSignature (FuncType (TCons (_, cName) tExprList@(_:_)) tExpr) =
  prettyPrintType tExprList : typeSignature tExpr

intercalate :: [a] -> [[a]] -> [a]
intercalate sep = concat . intersperse sep 