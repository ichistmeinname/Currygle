----------------------------------------------------------------------
--- Functions to generate documentation in "CDoc" format.
---
--- @author Sandra Dylus
----------------------------------------------------------------------

module CurryDocCDoc where

import CurryDocParams
import CurryDocRead
import FlatCurry
-- import FlexRigid
-- import Char
-- import AnaCompleteness
import ReadShowTerm



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
		   	     -- ,( map (\ (x,y) -> (x, splitComment y)) progcmts
		    	     -- , types
		    	     -- , functions
		    	     -- , ops
		    	     -- , anainfo)
			     )
		    	     
		    

-- auxilieres --------------------------------------------------------

versionOrAuthor :: String -> [(String, String)] -> String
versionOrAuthor string av = concat $ getCommentType string av
