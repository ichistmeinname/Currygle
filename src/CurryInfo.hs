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
import Control.Monad                  ( liftM5, liftM3, liftM2, liftM )

import Data.Binary                    ( Binary(..), putWord8, getWord8 )

-- | ModuleInfo holds information about the name, latest version, author,
--   list of imported modules and the description of a given module
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

-- | Data to represent the flexible and rigid attribute of a function
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

-- | Type for qualified names
type QName = (String, String)

-- empty constructor
emptyQName :: QName
emptyQName = ("","")

-- | This data represents a type expression and can occur as type variable with an index (Int),
--   a function type constructed with two type expressions or a type constructor with a qualified
--   and a list of type expressions.
data TypeExpr =
    TVar Int              
  | FuncType TypeExpr TypeExpr     
  | TCons QName [TypeExpr]  
  | Undefined   
  deriving (Read, Show)   

instance NFData TypeExpr where
    rnf (TVar i) = rnf i
    rnf (FuncType t1 t2) = rnf t1 `seq` rnf t2
    rnf (TCons name tList) = rnf name `seq` rnf tList
    rnf Undefined = ()

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

-- | FunctionInfo holds information about the name, signature, corresponding module,
--   description and flexible/rigid status of a function and its non-/deterministic behaviour
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

-- lifts six arguments
liftM6 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) 
                  -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6 = 
  do {x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; return (f x1 x2 x3 x4 x5 x6)}

instance NFData FunctionInfo where
    rnf (FunctionInfo n s m d nd fr) = rnf n `seq` rnf s `seq` rnf m `seq` rnf d `seq` rnf nd `seq` rnf fr

instance Binary FunctionInfo where
    put (FunctionInfo n s m d nd fr) = put n >> put s >> put m >> put d >> put nd >> put fr
    get = do
            r <- liftM6 FunctionInfo get get get get get get
            rnf r `seq` return r

-- | TypeInfo holds information about the name, signature, corresponding module,
--   and description of a given type
data TypeInfo = TypeInfo String [(QName, [TypeExpr])] [Int] String String deriving (Show, Read)

tName :: TypeInfo -> String
tName (TypeInfo n _ _ _ _) = n

tSignature :: TypeInfo -> [(QName, [TypeExpr])]
tSignature (TypeInfo _ s _ _ _) = s

tVarIndex :: TypeInfo -> [Int]
tVarIndex (TypeInfo _ _ v _ _) = v

tModule :: TypeInfo -> String
tModule (TypeInfo _ _ _ m _) = m

tDescription :: TypeInfo -> String
tDescription (TypeInfo _ _ _ _ d) = d

instance NFData TypeInfo where
    rnf (TypeInfo n s v m d) = rnf n `seq` rnf s `seq` rnf v `seq` rnf m `seq` rnf d

instance Binary TypeInfo where
    put (TypeInfo n s v m d) = put n >> put s >> put v >> put m >> put d
    get = do
            r <- liftM5 TypeInfo get get get get get
            rnf r `seq` return r
          
-- | The CurryInfo data holds information about the module, and corresponding functions,
--   data and type declaration of a given 'curry file'
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

-- | Empty constructor for CurryInfo
emptyCurryInfo   :: CurryInfo
emptyCurryInfo   = mkCurryInfo emptyModuleInfo [emptyFunctionInfo] [emptyTypeInfo]

mkCurryInfo  :: ModuleInfo -> [FunctionInfo] -> [TypeInfo] -> CurryInfo
mkCurryInfo  = CurryInfo

-- | Empty constructor for ModuleInfo
emptyModuleInfo :: ModuleInfo
emptyModuleInfo = mkModuleInfo "" "" "" [""] ""

mkModuleInfo :: String -> String -> String -> [String] -> String -> ModuleInfo
mkModuleInfo = ModuleInfo

-- | Empty constructor for FunctionInfo
emptyFunctionInfo :: FunctionInfo
emptyFunctionInfo = mkFunctionInfo "" (emptyQName, Undefined) "" "" False UnknownFR

mkFunctionInfo :: String -> (QName, TypeExpr) -> String -> String -> Bool -> FlexRigidResult -> FunctionInfo
mkFunctionInfo = FunctionInfo

-- | Empty constructor for TypeInfo
emptyTypeInfo :: TypeInfo
emptyTypeInfo = mkTypeInfo "" [] [] "" ""

mkTypeInfo :: String -> [(QName, [TypeExpr])] -> [Int] -> String -> String -> TypeInfo
mkTypeInfo = TypeInfo