module Rejig.Settings where

import Control.Monad.Reader

data ImportDeclQualifiedStyleSource
  = QualifiedPre
  | QualifiedPost
    deriving (Show, Eq)

data SourceLang
  = Haskell
  | Daml
    deriving (Show, Eq)

type AppSettings = Reader Settings Settings

data Settings = Settings
 { qualifiedStyle :: ImportDeclQualifiedStyleSource
 }
  deriving (Show, Eq)
