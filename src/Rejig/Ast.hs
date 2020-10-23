{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rejig.Ast where

import qualified Text.Show
import qualified Data.Text as T

data Comment
  = SingleLineComment Text
  -- ^ -- a single line
  | BlockComment Text
  -- ^ {- a block comment -}
  | CommentNewLine
  -- ^ seperator to detect blocks of multiline single comments
    deriving (Show, Eq)

-- ^ Represents source code that begins with a leading comment then something else.
data LeadingCommentedThing a = LeadingCommentedThing
 { _leadingComments :: [Comment]
 , _leadingThing :: a
 } deriving (Show, Eq, Functor)

-- {-# LANGUAGE <Extension> #-}
newtype LangExt = LangExt {unLangExt :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- {-# OPTIONS_GHC -Wno-all-case #-}
newtype GhcOption = GhcOption {unGhcOption :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- | Inspired by https://hackage.haskell.org/package/ghc-lib-parser-8.10.1.20200324/docs/GHC-Hs-ImpExp.html#t:ImportDecl
data ImportDecl = ImportDecl
  { ideclName :: QConId
    -- ^ A ModuleName is essentially a string e.g. Data.List
  , ideclPkgQual :: Maybe Text
    -- ^ Package qualifier
  , ideclIsQual :: Bool
    -- ^ Does the qualified keyword appear
  , ideclAs :: Maybe QConId
    -- ^ as Module
  , ideclHiding :: Maybe (Bool, [IE])
    -- ^ (True => hiding, names)
  }
  deriving (Show, Eq)

data PartitionedImports = PartitionedImports
  { _piRest :: CG ImportDeclGroups
    -- ^ catch all 'standard imports' that dont fall into the other groups
  , _piPrefixTargets :: [CG ImportDeclGroups]
    -- ^ imports grouped by user defined setting
  , _piPkgQuals :: CG ImportDeclGroups
    -- ^ contains package qualified imports in the form - import "pkg" ...
  }
  deriving (Show, Eq)

-- | CG = Comment group, its purpose is to attach a comment to the start of a group which acts as a title.
data CG a = CG
  { _cgComment :: Maybe Text,
    _cgGroup :: a
  }
  deriving (Show, Eq, Functor)

-- Defining lots of newtypes for things to implement 'Pretty' typeclass instance.

newtype ImportDecls = ImportDecls {unImportDecls :: [ImportDecl]}
  deriving newtype (Show, Eq)

newtype ImportDeclGroups = ImportDeclGroups {unImportDeclGroups :: [ImportDecls]}
  deriving newtype (Show, Eq)

newtype QVarId = QVarId { unQVarId :: Qual }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype QVarSym = QVarSym {unQVarSym :: Qual }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype ConId = ConId {unConId :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype QConId = QConId {unQConId :: Qual}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype QConSym = QConSym {unQConSym :: Qual}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

data Qual = Qual [ConId] Text
  deriving (Eq)

instance Show Qual where
  show (Qual modids x) =
    T.unpack . T.concat . intersperse "." $ (map unConId modids) ++ [x]

instance Ord Qual where
  compare a b = compare (show a :: String) (show b :: String)

data CName
  = CVarId QVarId
  | CVarSym QVarSym
  | CConId QConId
  | CConSym QConSym
  deriving (Show, Eq)

cnameIndex :: CName -> Int
cnameIndex = \case
  CVarId _ -> 0
  CVarSym _ -> 1
  CConId _ -> 2
  CConSym _ -> 3

data QVar
  = VId QVarId
  | VSym QVarSym
  deriving (Show, Eq)

varIndex :: QVar -> Int
varIndex = \case
  VId _ -> 0
  VSym _ -> 1

{- Since the comparison is done on the inner wrapped type, custom `Ord` instances are defined.
   The final catch all case uses a helper function that avoids needing to define every permutation.
-}

instance Ord QVar where
  compare (VId a) (VId b) = compare a b
  compare (VSym a) (VSym b) = compare a b
  compare a b = compare (varIndex a) (varIndex b)

instance Ord CName where
  compare (CVarId a) (CVarId b) = compare a b
  compare (CVarSym a) (CVarSym b) = compare a b
  compare (CConId a) (CConId b) = compare a b
  compare (CConSym a) (CConSym b) = compare a b
  compare a b = compare (cnameIndex a) (cnameIndex b)

-- | Inspired by https://downloads.haskell.org/~ghc/7.2.1/docs/html/libraries/ghc-7.2.1/HsImpExp.html
data IE
  = IEVar QVar
  -- ^ Imported or Exported Variable
  | IEThingAbs QConId
  -- ^ Class/Type - cant tell eg: Month
  | IEThingAll QConId
  -- ^ Class/Type plus all methods/constructors, eg: Month (..)
  | IEThingWith QConId [CName]
  -- ^ Class/Type plus some methods/constructors eg: Month (Jan, Feb)
  | IEModuleContents QConId
    -- ^ module xyz - exports only
  deriving (Show, Eq)

ieIndex :: IE -> Int
ieIndex = \case
  IEVar _ -> 0
  IEThingAbs _ -> 1
  IEThingAll _ -> 2
  IEThingWith _ _ -> 3
  IEModuleContents _ -> 4

instance Ord IE where
  compare (IEVar a) (IEVar b) = compare a b
  compare (IEThingAbs a) (IEThingAbs b) = compare a b
  compare (IEThingAll a) (IEThingAll b) = compare a b
  compare (IEThingWith conidA namesA) (IEThingWith conidB namesB) =
    compare conidA conidB <> compare namesA namesB
  compare (IEModuleContents a) (IEModuleContents b) =
    compare a b
  compare a b = compare (ieIndex a) (ieIndex b)

-- Initial parse result
data ModuleHeader = ModuleHeader
  { _modGhcOpts :: [GhcOption]
  , _modLangExts :: [LangExt]
  , _modName :: QConId
  , _modExports :: [IE]
  , _modImports :: ImportDecls
  }
  deriving (Show, Eq)

data SortedModuleHeader = SortedModuleHeader
  { _smodLangExts :: [LangExt]
  , _smodGhcOpts :: [GhcOption]
  , _smodName :: QConId
  , _smodExports :: [IE]
  , _smodImports :: PartitionedImports
  }
  deriving (Show, Eq)

data ParsedSource = ParsedSource
 { _srcModHeader :: LeadingCommentedThing ModuleHeader
 , _srcRest :: Text
 }
  deriving (Show, Eq)

-- This tool only formats the mod header, the rest of the source code remains untouched
data SortedParsedSource = SortedParsedSource
 { _ssrcModHeader :: LeadingCommentedThing SortedModuleHeader
 , _ssrcRest :: Text
 }
  deriving (Show, Eq)
