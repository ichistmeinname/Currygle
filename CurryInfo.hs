-- Created by Sandra Dylus on 12.05.2012

-- first idea for the information associated with a curry module, function and type

-- | Information about a curry module, function and type.

data ModuleInfo     = ModuleInfo
                      { mName         :: String           -- ^ the name
                      , mVersion      :: String           -- ^ the latest version
                      , mAuthor       :: String           -- ^ the author
                      , mImports      :: [String]         -- ^ list of the imported modules
                      , mDescription  :: String           -- ^ the description as Comment String
                      } deriving (Show, Read)

showM n v a i d = show ModuleInfo {mName = n, mVersion = v, mAuthor = a, mImports = i, mDescription = d} 


-- data CurryInfo   = CurryInfo      
--                    { c_name         :: String           -- ^ the name
--                    , c_definition   :: SourceLine       -- ^ the signature as ModDef, FuncDef String or DataDef String
--                    , c_module       :: SourceLine       -- ^ the corresponding module as ModDef
--                    , c_description  :: SourceLine       -- ^ the description as Comment String
--                    }


data FunctionInfo   = FunctionInfo      
                      { f_name         :: String           -- ^ the name
                      , f_signature    :: [String]         -- ^ the signature
                      , f_module       :: String           -- ^ the corresponding module
                      , f_description  :: String           -- ^ the description
                      , f_nondet       :: Bool             -- ^ 'True' if property is definied non-deterministically
                      , f_flexRigid    :: FlexRigidResult  -- ^ the flex/rigid status 
                      }

data TypeInfo       = TypeInfo
                      { t_name         :: String           -- ^ the name
                      , t_signature    :: SourceLine       -- ^ the signature as DataDef String
                      , t_module       :: SourceLine       -- ^ the corresponding module as ModDef
                      , t_description  :: SourceLine       -- ^ the description as Comment String
                      }

data SourceLine = Comment String  -- a comment for CurryDoc
                | FuncDef String  -- a definition of a function
                | DataDef String  -- a definition of a datatype
                | ModDef          -- a line containing a module definition
                | OtherLine       -- a line not relevant for CurryDoc
                  deriving (Show, Read)

data FlexRigidResult = UnknownFR | ConflictFR | KnownFlex | KnownRigid


-- type Test = [(SourceLine, (String, [(String, String)]))]


-- | auxilieres 


test :: FilePath -> IO String
test a = do
       text <- readFile a
       (n, v, ana, i, d) <- readIO text 
       return $ showM n v ana i d


file = "../../../../../Desktop/DOC_firstprog/firstprog.cdoc"

showSome = show ModuleInfo {mName = "RecordTest", mVersion = "0.1", mAuthor = "Sandra Dylus", mImports = [""], mDescription = "Ein Kommentar zum Test"}
       
