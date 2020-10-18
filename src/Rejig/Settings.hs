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
 { _sQualifiedStyle :: ImportDeclQualifiedStyleSource
 , _sGroupByPrefix :: [Text]
 , _sShowGroupComments :: Bool
 , _sImportBorderTop :: Bool
 , _sImportBorderBottom :: Bool
 }
  deriving (Show, Eq)
