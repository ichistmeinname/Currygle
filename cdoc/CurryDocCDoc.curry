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
generateCDoc progname modcmts progcmts anainfo = do
	     let fcyname = flatCurryFileName progname
  	     putStrLn $ "Reading FlatCurry program \""++fcyname++"\"..."
  	     (Prog modname imports types functions ops) <- readFlatCurryFile fcyname
	     return $
	     	    showTerm ((\ (mod,av) -> ( modname 
			       	  		    , versionOrAuthor "version" av
						    , versionOrAuthor "author" av
						    , imports
						    , mod
						    ))  $ splitComment modcmts
		    	     ,  map (funcInfo anainfo progcmts) functions 
		    	     -- , ops
		    	     -- , types
			     )
			     -- where splitFuncDecl (Func (mName, fName) _ _ tExpr (Rule _ expr)) = 
	      		     -- 	   (fName, splitTypeExpr tExpr, mName, getFuncComment fName progcmts, getOverlappingInfo anainfo fName, getFlexRigid expr)

-- auxilieres --------------------------------------------------------

versionOrAuthor :: String -> [(String, String)] -> String
versionOrAuthor string av = concat $ getCommentType string av

-- generates all needed informations about a function
funcInfo :: AnaInfo -> [(SourceLine, String)] -> FuncDecl -> (String, String, String, String, Bool, FlexRigidResult)
funcInfo anainfo progcmts (Func (mName, fName) _ _ tExpr (Rule _ expr)) = (fName, splitTypeExpr tExpr, mName, getFuncComment fName progcmts, getOverlappingInfo anainfo (mName, fName), getFlexRigid expr)

-- generates type signature 
splitTypeExpr :: TypeExpr -> String
splitTypeExpr (TCons (_, cName) tExprList)   	  	    = unwrap cName tExprList
splitTypeExpr (FuncType (TCons (_, cName) tExprList) tExpr) = unwrap cName tExprList ++ ("->" ++ (splitTypeExpr tExpr))

-- WARNING: TUPELS!!!
-- help function for nested types like [Int] or (String, String) (recursive!)
unwrap :: String -> [TypeExpr] -> String
unwrap cName tExprList  = 
       	     	  let b = if cName == "[]" then "[" ++ concatMap splitTypeExpr tExprList ++ "]" else "(" ++ concatMap splitTypeExpr tExprList ++ ")" 
		      	  -- case cName of
		      	  -- "[]" 	    -> "[" ++ concatMap splitTypeExpr tExprList ++ "]"
		      	  -- tupelList -> "(" ++ concatMap splitTypeExpr tExprList ++ ")" -- still a bug in tupels!
		      c = if null tExprList then cName else b
		  in c
		  -- in if null tExprList then cName else b 
 