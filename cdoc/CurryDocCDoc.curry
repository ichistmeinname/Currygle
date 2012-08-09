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
    Prog modName imports types functions _ <- readFlatCurryFile fcyName
    let modInfo = ModuleInfo 
	    modName 
	    (versionOrAuthor "version" avCmts) 
	    (versionOrAuthor "author" avCmts) 
	    imports mCmts
	funcInfo (Func (mName, fName) _ _ tExpr (Rule _ expr)) = 
            FunctionInfo fName 
	    ((mName, fName), tExpr) 
	    mName 
	    (funcComment fName progCmts)
	    (getOverlappingInfo anaInfo (modName, fName)) 
	    (getFlexRigid expr)
	funcInfo (Func (mName, fName) _ _ tExpr (External _)) = 
            FunctionInfo fName 
	    ((mName, fName), tExpr)
	    mName 
	    (funcComment fName progCmts)
	    (getOverlappingInfo anaInfo (modName, fName)) 
	    UnknownFR 
	typeInfo (Type (mName, tName) _ vars consDecl) = 
		       	       	   TypeInfo tName
	       	       	      	   (map consSignature (filter (\(Cons _ _ vis _) -> vis == Public) consDecl))
				   vars
				   mName
				   (dataComment tName progCmts)
        typeInfo (TypeSyn (mName, tName) _ vars tExpr) =
		 	  	  TypeInfo tName
				  [((mName, tName), [tExpr])]
				  vars
				  mName
				  (dataComment tName progCmts)
        (mCmts, avCmts) = splitComment modCmts
	funcInfos = map funcInfo (filter (\(Func _ _ vis _ _) -> vis == Public) functions)
	typeInfos = map typeInfo (concatMap filterT types)
    return $ showTerm (CurryInfo modInfo funcInfos typeInfos)
  where fcyName  = flatCurryFileName progName
  	filterT f@(Type _ vis _ _) = if vis == Public then [f] else []
	filterT f@(TypeSyn _ vis _ _) = if vis == Public then [f] else []

funcComment :: String -> [(SourceLine,String)] -> String
funcComment str = fst . splitComment . getFuncComment str 

dataComment :: String -> [(SourceLine,String)] -> String
dataComment str = fst . splitComment . getDataComment str

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
data FunctionInfo = FunctionInfo String (QName, TypeExpr) String String Bool FlexRigidResult

-- the name
-- the signature
-- the corresponding module
-- the description
data TypeInfo = TypeInfo String [(QName, [TypeExpr])] [TVarIndex] String String

-- auxilieres --------------------------------------------------------

versionOrAuthor :: String -> [(String, String)] -> String
versionOrAuthor string av = concat $ getCommentType string av

-- generate data and type constructors
consSignature :: ConsDecl -> (QName, [TypeExpr])
consSignature (Cons (mName, cName) _ _ tExprList) = ((mName, cName), tExprList)

