{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

newtype VarId = VarId Text
  deriving stock (Show)
  deriving newtype (Eq)

newtype VarSym = VarSym Text
  deriving stock (Show)
  deriving newtype (Eq)

newtype ConId = ConId Text
  deriving stock (Show)
  deriving newtype (Eq)

newtype ConSym = ConSym Text
  deriving stock (Show)
  deriving newtype (Eq)

data Qual = Qual [ConId] CName
  deriving (Show, Eq)

data CName
  = CVarId VarId
  | CVarSym VarSym
  | CConId ConId
  | CConSym ConSym
  deriving (Show, Eq)

data Var
  = VId VarId
  | VSym VarSym
  deriving (Show, Eq)

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

data ModuleDecl = ModuleDecl
 { mmodid :: Text
 , exports :: [ImportDecl]
 , body :: Text
 } deriving (Show, Eq)

data Program
  = LanguageExtensions [Text]
  | Module ModuleDecl
   deriving (Show, Eq)
