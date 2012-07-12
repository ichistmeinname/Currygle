{- |
Module      :  CurryInfo
Description :  Module for curry data type.
Copyright   :  (c) Sandra Dylus
License     :  <license>

Maintainer  :  sad@informatik.uni-kiel.de
Stability   :  experimental
Portability :  portable

Module for the information associated with a curry module, function and type.
-}

module CurryInfo where

import Control.DeepSeq
import Control.Monad                  ( liftM5, liftM4, liftM3, liftM2, liftM )

import Data.Binary                    ( Binary(..), putWord8, getWord8 )

import Helpers

-- | Information about a curry module, function and type.

emptyCurryInfo   :: CurryInfo
emptyCurryInfo   = mkCurryInfo emptyModuleInfo [emptyFunctionInfo] [emptyTypeInfo]

mkCurryInfo  :: ModuleInfo -> [FunctionInfo] -> [TypeInfo] -> CurryInfo
mkCurryInfo  = CurryInfo

emptyModuleInfo :: ModuleInfo
emptyModuleInfo = mkModuleInfo "" "" "" [""] ""

mkModuleInfo :: String -> String -> String -> [String] -> String -> ModuleInfo
mkModuleInfo = ModuleInfo

emptyFunctionInfo :: FunctionInfo
emptyFunctionInfo = mkFunctionInfo "" (emptyQName, Undefined) "" "" False UnknownFR

mkFunctionInfo :: String -> (QName, TypeExpr) -> String -> String -> Bool -> FlexRigidResult -> FunctionInfo
mkFunctionInfo = FunctionInfo

emptyTypeInfo :: TypeInfo
emptyTypeInfo = mkTypeInfo "" [] "" ""

mkTypeInfo :: String -> [(QName, [TypeExpr])] -> String -> String -> TypeInfo
mkTypeInfo = TypeInfo

-- the module
-- the corresponding functions
-- the corresponding data and type declaration
data CurryInfo = CurryInfo ModuleInfo [FunctionInfo] [TypeInfo] deriving (Show, Read)

moduleInfo :: CurryInfo -> ModuleInfo
moduleInfo (CurryInfo m _ _) = m

functionInfos :: CurryInfo -> [FunctionInfo]
functionInfos (CurryInfo _ f _) = f

typeInfos :: CurryInfo -> [TypeInfo]
typeInfos (CurryInfo _ _ t) = t

instance NFData CurryInfo where
    rnf (CurryInfo m f t) = rnf m `seq` rnf f `seq` rnf t

instance Binary CurryInfo where
    put (CurryInfo m f t) = put m >> put f >> put t
    get = do
            r <- liftM3 CurryInfo get get get
            rnf r `seq` return r

-- the name
-- the latest version
-- the author
-- list of the imported modules
-- the description
data ModuleInfo = ModuleInfo String String String [String] String deriving (Show, Read)

mName :: ModuleInfo -> String
mName (ModuleInfo n _ _ _ _) = n

mVersion :: ModuleInfo -> String
mVersion (ModuleInfo _ v _ _ _) = v

mAuthor :: ModuleInfo -> String
mAuthor (ModuleInfo _ _ a _ _) = a

mImports :: ModuleInfo -> [String]
mImports (ModuleInfo _ _ _ i _) = i

mDescription :: ModuleInfo -> String
mDescription (ModuleInfo _ _ _ _ d) = d

instance NFData ModuleInfo where
    rnf (ModuleInfo n v a i d) = rnf n `seq` rnf v `seq` rnf a `seq` rnf i `seq` rnf d

instance Binary ModuleInfo where
    put (ModuleInfo n v a i d) = put n >> put v >> put a >> put i >> put d
    get = do
            r <- liftM5 ModuleInfo get get get get get
            rnf r `seq` return r

-- the name
-- the signature
-- the corresponding module
-- the description
-- True if property ist defined non-deterministically
-- the flex/rigid status
data FunctionInfo = FunctionInfo String (QName, TypeExpr) String String Bool FlexRigidResult deriving (Show, Read)

fName :: FunctionInfo -> String
fName (FunctionInfo n _ _ _ _ _) = n

fSignature :: FunctionInfo -> (QName, TypeExpr)
fSignature (FunctionInfo _ s _ _ _ _) = s

fModule :: FunctionInfo -> String
fModule (FunctionInfo _ _ m _ _ _) = m

fDescription :: FunctionInfo -> String
fDescription (FunctionInfo _ _ _ d _ _) = d

fNonDet :: FunctionInfo -> Bool
fNonDet (FunctionInfo _ _ _ _ nd _) = nd

fFlexRigid :: FunctionInfo -> FlexRigidResult
fFlexRigid (FunctionInfo _ _ _ _ _ fr) = fr  

instance NFData FunctionInfo where
    rnf (FunctionInfo n s m d nd fr) = rnf n `seq` rnf s `seq` rnf m `seq` rnf d `seq` rnf nd `seq` rnf fr

instance Binary FunctionInfo where
    put (FunctionInfo n s m d nd fr) = put n >> put s >> put m >> put d >> put nd >> put fr
    get = do
            r <- liftM6 FunctionInfo get get get get get get
            rnf r `seq` return r

-- the name
-- the signature
-- the corresponding module
-- the description
data TypeInfo = TypeInfo String [(QName, [TypeExpr])] String String deriving (Show, Read)

tName :: TypeInfo -> String
tName (TypeInfo n _ _ _) = n

tSignature :: TypeInfo -> [(QName, [TypeExpr])]
tSignature (TypeInfo _ s _ _) = s

tModule :: TypeInfo -> String
tModule (TypeInfo _ _ m _) = m

tDescription :: TypeInfo -> String
tDescription (TypeInfo _ _ _ d) = d

instance NFData TypeExpr where
    rnf (TVar i) = rnf i
    rnf (FuncType t1 t2) = rnf t1 `seq` rnf t2
    rnf (TCons name tList) = rnf name `seq` rnf tList

instance Binary TypeExpr where
    put (TVar i) = putWord8 0 >> put i
    put (FuncType t1 t2) = putWord8 1 >> put t1 >> put t2
    put (TCons name tList) = putWord8 2 >> put name >> put tList
    put Undefined = putWord8 3
    get = do 
          tag <- getWord8
          case tag of
            0 -> liftM TVar get
            1 -> liftM2 FuncType get get
            2 -> liftM2 TCons get get
            _ -> return Undefined

instance NFData TypeInfo where
    rnf (TypeInfo n s m d) = rnf n `seq` rnf s `seq` rnf m `seq` rnf d

instance Binary TypeInfo where
    put (TypeInfo n s m d) = put n >> put s >> put m >> put d
    get = do
            r <- liftM4 TypeInfo get get get get
            rnf r `seq` return r

data FlexRigidResult = UnknownFR | ConflictFR | KnownFlex | KnownRigid deriving (Show, Read)

instance NFData FlexRigidResult 

instance Binary FlexRigidResult where
      put UnknownFR = putWord8 0
      put ConflictFR = putWord8 1
      put KnownFlex = putWord8 2
      put KnownRigid = putWord8 3
      get = do
        tag <- getWord8
        case tag of
          1 -> return ConflictFR
          2 -> return KnownFlex
          3 -> return KnownRigid
          _ -> return UnknownFR
          
loadFromCurryFile :: FilePath -> IO CurryInfo
loadFromCurryFile a = do
       text <- readFile a
       CurryInfo m f t <- readIO text 
       return $ (CurryInfo m f t)

liftM6 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6 = do {x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; return (f x1 x2 x3 x4 x5 x6)}

filePath :: FilePath
filePath = "../resources/static"
