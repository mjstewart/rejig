module Rejig.Settings where

data ImportDeclQualifiedStyleSource
  = QualifiedPre
  | QualifiedPost
    deriving (Show, Eq)

data SourceLang
  = Haskell
  | Daml
    deriving (Show, Eq)

data Settings = Settings
 { _sQualifiedStyle :: ImportDeclQualifiedStyleSource
 , _sGroupByPrefix :: [Text]
 , _sDisplayGroupTitle :: Bool
 , _sImportBorderTop :: Bool
 , _sImportBorderBottom :: Bool
 }
  deriving (Show, Eq)
