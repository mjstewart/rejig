{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Rejig.Ast where

-- {-# LANGUAGE <Extension> #-}
newtype Pragma = Pragma { unPragma :: Text }
  deriving (Show, Eq)

-- {-# OPTIONS_GHC -Wno-all-case #-}
newtype GhcOption = GhcOption { unGhcOption :: Text }
  deriving (Show, Eq)

-- | Inspired by https://hackage.haskell.org/package/ghc-lib-parser-8.10.1.20200324/docs/GHC-Hs-ImpExp.html#t:ImportDecl
data ImportDecl = ImportDecl
 { ideclName :: Qual
 -- ^ A ModuleName is essentially a string e.g. Data.List
 , ideclPkgQual :: Maybe Text
 -- ^ Package qualifier
 , ideclIsQual :: Bool
 -- ^ Does the qualified keyword appear
 , ideclAs :: Maybe Qual
 -- ^ as Module
 , ideclHiding :: Maybe (Bool, [IE])
 -- ^ (True => hiding, names)
 } deriving (Show, Eq)


data PartitionedImports = PartitionedImports
 { _piRest :: CG ImportDeclGroups
 , _piPrefixTargets :: [CG ImportDeclGroups]
 , _piPkgQuals :: CG ImportDeclGroups
 } deriving (Show, Eq)

data CG a = CG
 { _cgComment :: Maybe Text
 , _cgGroup :: a
 } deriving (Show, Eq, Functor)

newtype ImportDecls = ImportDecls { unImportDecls :: [ImportDecl] }
  deriving newtype (Show, Eq)

newtype ImportDeclGroups = ImportDeclGroups { unImportDeclGroups :: [ImportDecls] }
  deriving newtype (Show, Eq)

newtype VarId = VarId { unVarId :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype VarSym = VarSym { unVarSym :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype ConId = ConId { unConId :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype ConSym = ConSym { unConSym :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

data Qual = Qual [ConId] CName
  deriving (Show, Eq)

data CName
  = CVarId VarId
  | CVarSym VarSym
  | CConId ConId
  | CConSym ConSym
  deriving (Show, Eq)

cnameIndex :: CName -> Int
cnameIndex = \case
  CVarId _ -> 0
  CVarSym _ -> 1
  CConId _ -> 2
  CConSym _ -> 3

data Var
  = VId VarId
  | VSym VarSym
  deriving (Show, Eq)

varIndex :: Var -> Int
varIndex = \case
  VId _ -> 0
  VSym _ -> 1

-- custom instances are needed since the comparison is done on the wrapped type.
-- The {type}Index function exists to use the overall ordering to avoid having to
-- declare every permutation.

instance Ord Var where
  compare (VId a) (VId b) = compare a b
  compare (VSym a) (VSym b) = compare a b
  compare a b = compare (varIndex a) (varIndex b)

instance Ord CName where
  compare (CVarId a) (CVarId b) = compare a b
  compare (CVarSym a) (CVarSym b) = compare a b
  compare (CConId a) (CConId b) = compare a b
  compare (CConSym a) (CConSym b) = compare a b
  compare a b = compare (cnameIndex a) (cnameIndex b)

data IE
  = IEVar Var
  -- ^ Imported or Exported Variable
  | IEThingAbs ConId
  -- ^ Imported or exported Thing with Absent list, eg: Month ()
  | IEThingAll ConId
  -- ^ ClassType plus all methods/constructors, eg: Month(..)
  | IEThingWith ConId [CName]
  -- ^ ClassType plus some methods/constructors eg: Month(Jan, Feb)
    deriving (Show, Eq)

ieIndex :: IE -> Int
ieIndex = \case
  IEVar _ -> 0
  IEThingAbs _ -> 1
  IEThingAll _ -> 2
  IEThingWith _ _ -> 3

instance Ord IE where
  compare (IEVar a) (IEVar b) = compare a b
  compare (IEThingAbs a) (IEThingAbs b) = compare a b
  compare (IEThingAll a) (IEThingAll b) = compare a b
  compare (IEThingWith conidA namesA) (IEThingWith conidB namesB) =
    compare conidA conidB <> compare namesA namesB


data ModuleDecl = ModuleDecl
 { mmodid :: Text
 , exports :: [ImportDecl]
 , body :: Text
 } deriving (Show, Eq)

data Program
  = LanguageExtensions [Text]
  | Module ModuleDecl
   deriving (Show, Eq)
