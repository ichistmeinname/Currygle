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
import ReadShowTerm
import List

generateCDoc :: String  -> String -> [(SourceLine,String)] -> AnaInfo -> IO String
generateCDoc progName modCmts progCmts anaInfo = do
    putStrLn $ "Reading FlatCurry program \""++fcyName++"\"..."
    Prog modName imports types functions ops <- readFlatCurryFile fcyName
    let modInfo = ModuleInfo 
	    modName 
	    (versionOrAuthor "version" avCmts) 
	    (versionOrAuthor "author" avCmts) 
	    imports mCmts
	funcInfo (Func (mName, fName) _ _ tExpr (Rule _ expr)) = 
            FunctionInfo fName 
	    (typeSignature tExpr) 
	    mName 
	    (getFuncComment fName progCmts) 
	    (getOverlappingInfo anaInfo (modName, fName)) 
	    (getFlexRigid expr) 
	typeInfo (Type (mName, tName) _ _ consDecl) = 
		       	       	   TypeInfo tName
	       	       	      	   (map consSignature consDecl)	
				   mName
				   (getDataComment tName progCmts)
        typeInfo (TypeSyn (mName, tName) _ _ tExpr) =
		 	  	  TypeInfo tName
				  ([typeSignature tExpr])
				  (mName)
				  (getDataComment tName progCmts)
        (mCmts, avCmts) = splitComment modCmts
	funcInfos = map funcInfo functions
	typeInfos = map typeInfo types
    return $ showTerm (CurryInfo modInfo funcInfos typeInfos)
  where fcyName  = flatCurryFileName progName


-- the name
-- the latest version
-- the author
-- list of the imported modules
-- the description
data ModuleInfo = ModuleInfo String String String [String] String

-- the module
-- the corresponding functions
-- the corresponding data and type declaration
data CurryInfo = CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]

-- the name
-- the signature
-- the corresponding module
-- the description
-- True if property ist defined non-deterministically
-- the flex/rigid status
data FunctionInfo = FunctionInfo String [String] String String Bool FlexRigidResult

-- the name
-- the signature
-- the corresponding module
-- the description
data TypeInfo = TypeInfo String [[String]] String String

-- auxilieres --------------------------------------------------------

versionOrAuthor :: String -> [(String, String)] -> String
versionOrAuthor string av = concat $ getCommentType string av

-- pretty print for special types like lists or tupels
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
typeSignature (FuncType (TCons (_, _) tExprList@(_:_)) tExpr) =
  prettyPrintType tExprList : typeSignature tExpr

-- generate data and type constructors
consSignature :: ConsDecl -> [String]
consSignature (Cons (_, cName) _ _ tExprList) = cName : concatMap typeSignature tExprList

intercalate :: [a] -> [[a]] -> [a]
intercalate sep = concat . intersperse sep 