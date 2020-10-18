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
    deriving (Show, Eq)

-- ^ Represents source code with a leading comment then something else.
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
  { -- | A ModuleName is essentially a string e.g. Data.List
    ideclName :: QConId,
    -- | Package qualifier
    ideclPkgQual :: Maybe Text,
    -- | Does the qualified keyword appear
    ideclIsQual :: Bool,
    -- | as Module
    ideclAs :: Maybe QConId,
    -- | (True => hiding, names)
    ideclHiding :: Maybe (Bool, [IE])
  }
  deriving (Show, Eq)

data PartitionedImports = PartitionedImports
  { _piRest :: CG ImportDeclGroups,
    _piPrefixTargets :: [CG ImportDeclGroups],
    _piPkgQuals :: CG ImportDeclGroups
  }
  deriving (Show, Eq)

data CG a = CG
  { _cgComment :: Maybe Text,
    _cgGroup :: a
  }
  deriving (Show, Eq, Functor)

newtype ImportDecls = ImportDecls {unImportDecls :: [ImportDecl]}
  deriving newtype (Show, Eq)

newtype ImportDeclGroups = ImportDeclGroups {unImportDeclGroups :: [ImportDecls]}
  deriving newtype (Show, Eq)

newtype QVarId = QVarId { unQVarId :: Qual }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- newtype VarId = VarId {unVarId :: Text}
  -- deriving stock (Show)
  -- deriving newtype (Eq, Ord)


-- newtype VarSym = VarSym {unVarSym :: Text}
  -- deriving stock (Show)
  -- deriving newtype (Eq, Ord)

newtype QVarSym = QVarSym {unQVarSym :: Qual }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype ConId = ConId {unConId :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype QConId = QConId {unQConId :: Qual}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- newtype ConSym = ConSym {unConSym :: Text}
  -- deriving stock (Show)
  -- deriving newtype (Eq, Ord)

newtype QConSym = QConSym {unQConSym :: Qual}
  deriving stock (Show)
  deriving newtype (Eq, Ord)


data Qual = Qual [ConId] Text
  deriving (Eq)

instance Show Qual where
  show (Qual modids x) =
    T.unpack . T.concat . intersperse "." $ (map unConId modids) ++ [x]

instance Ord Qual where
  compare a b = compare (show a) (show b)

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

-- custom instances are needed since the comparison is done on the wrapped type.
-- The {type}Index function exists to use the overall ordering to avoid having to
-- declare every permutation.

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

data IE
  = -- | Imported or Exported Variable
    IEVar QVar
  | -- | Imported or exported Thing with Absent list, eg: Month ()
    IEThingAbs QConId
  | -- | ClassType plus all methods/constructors, eg: Month(..)
    IEThingAll QConId
  | -- | ClassType plus some methods/constructors eg: Month(Jan, Feb)
    IEThingWith QConId [CName]
  | IEModuleContents QConId
    -- ^ module xyz
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

-- data EThing
  -- = EThingVar QVar
    -- -- ^ Exported Variable
  -- | EThingAll QConId
    -- -- ^ ClassType plus all methods/constructors, eg: Month(..)
  -- | EThingWith QConId [CName]
    -- -- ^ ClassType plus some methods/constructors eg: Month(Jan, Feb)
  -- deriving (Show, Eq)

data ModuleHeader = ModuleHeader
  { _modLangExts :: [LangExt]
  , _modGhcOpts :: [GhcOption]
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

data SortedParsedSource = SortedParsedSource
 { _ssrcModHeader :: LeadingCommentedThing SortedModuleHeader
 , _ssrcRest :: Text
 }
  deriving (Show, Eq)

-- data Program
  -- = LanguageExtensions [Text]
  -- | Module ModuleDecl
  -- deriving (Show, Eq)
