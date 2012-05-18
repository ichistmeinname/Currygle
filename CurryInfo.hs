-- Created by Sandra Dylus on 12.05.2012

-- first idea for the information associated with a curry module, function and type

-- | Information about a curry module, function and type.

-- the name
-- the latest version
-- the author
-- list of the imported modules
-- the description
data ModuleInfo = ModuleInfo String String String [String] String deriving (Show, Read)

-- the module
-- the corresponding functions
-- the corresponding data and type declaration
data CurryInfo = CurryInfo ModuleInfo [FunctionInfo] [TypeInfo] deriving (Show, Read)

-- the name
-- the signature
-- the corresponding module
-- the description
-- True if property ist defined non-deterministically
-- the flex/rigid status
data FunctionInfo = FunctionInfo String [String] String String Bool FlexRigidResult deriving (Show, Read)

-- the name
-- the signature
-- the corresponding module
-- the description
data TypeInfo       = TypeInfo String [[String]] String String deriving (Show, Read)

-- records don't work in curry
-- data ModuleInfo   = ModuleInfo
--                     { mName         :: String           -- ^ the name
--                     , mVersion      :: String           -- ^ the latest version
--                     , mAuthor       :: String           -- ^ the author
--                     , mImports      :: [String]         -- ^ list of the imported modules
--                     , mDescription  :: String           -- ^ the description as Comment String
--                     } deriving (Show, Read)


-- data CurryInfo    = CurryInfo      
--                     { cModule      :: ModuleInfo        -- ^ the curry module
--                     , cFunctions   :: [FunctionInfo]    -- ^ the corresponding functions
--                     , cTypes       :: [TypeInfo]        -- ^ the corresponding types     
--                     } deriving (Show, Read)


-- data FunctionInfo = FunctionInfo      
--                     { fName         :: String           -- ^ the name
--                     , fSignature    :: [String]         -- ^ the signature
--                     , fModule       :: String           -- ^ the corresponding module
--                     , fDescription  :: String           -- ^ the description
--                     , fNondet       :: Bool             -- ^ 'True' if property is definied non-deterministically
--                     , fFlexRigid    :: FlexRigidResult  -- ^ the flex/rigid status 
--                     } deriving (Show, Read)

-- data TypeInfo     = TypeInfo
--                     { tName         :: String           -- ^ the name
--                     , tSignature    :: [[String]]       -- ^ the signature
--                     , tModule       :: String           -- ^ the corresponding module
--                     , tDescription  :: String           -- ^ the description
--                     } deriving (Show, Read)

data FlexRigidResult = UnknownFR | ConflictFR | KnownFlex | KnownRigid deriving (Show, Read)

-- | auxilieres 


test :: FilePath -> IO String
test a = do
       text <- readFile a
       CurryInfo m f t <- readIO text 
       return $ show (CurryInfo m f t)


file = "../../../../../Desktop/DOC_firstprog/firstprog.cdoc"
       
