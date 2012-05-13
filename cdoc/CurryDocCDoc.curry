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
import Char
import AnaCompleteness


-- generateCDoc :: String  -> String -> [(SourceLine,String)] -> AnaInfo -> [String] -> IO (String, (String, [(String,String)]), [(SourceLine, (String, [(String,String)]))], [TypeDecl], [FuncDecl], [OpDecl], AnaInfo, [String])
generateCDoc :: String  -> String -> [(SourceLine,String)] -> AnaInfo -> IO ([String], (String, (String, [(String,String)]), [(SourceLine, (String, [(String,String)]))]))
generateCDoc progname modcmts progcmts anainfo = do
	     let fcyname = flatCurryFileName progname
  	     putStrLn $ "Reading FlatCurry program \""++fcyname++"\"..."
  	     (Prog modname imports types functions ops) <- readFlatCurryFile fcyname
	     return $
	     	    (imports
		    ,
		    ( modname
		    , splitComment modcmts
		    , map (\(x,y) -> (x, splitComment y)) progcmts
		    -- , types
		    -- , functions
		    -- , ops
		    -- , anainfo
		    ) )
		    

