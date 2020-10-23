module Rejig.Settings where

data ImportDeclQualifiedStyleSource
  = QualifiedPre
  | QualifiedPost
    deriving (Show, Eq)

data SourceLang
  = Haskell
  | Daml
    deriving (Show, Eq)

data Input
  = FileInput FilePath
  | StdInput String
  -- ^ Additional text is to help with any error reporting to describe
  -- what the input is associated to, eg file name.
   deriving (Show, Eq)

data Settings = Settings
 { _sInput :: Input
 , _ssrcLang :: SourceLang
 , _sPrefixGroups :: [Text]
 , _sDisplayGroupTitle :: Bool
 , _sImportBorderTop :: Bool
 , _sImportBorderBottom :: Bool
 }
  deriving (Show, Eq)
