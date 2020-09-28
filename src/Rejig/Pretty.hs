{-# LANGUAGE MultiParamTypeClasses #-}


module Rejig.Pretty where

import Rejig.Ast
import qualified Data.Set as Set
import qualified Data.Text as Text
import Text.PrettyPrint
import Prelude hiding (empty)
import Control.Monad.Reader
import Rejig.Settings

vgroups :: [[Doc]] -> Doc
vgroups =
  sep . (vcat <$>) . intersperse [newline]

newline :: Doc
newline =
  text ""

ttext :: Text -> Doc
ttext = text . Text.unpack

dot :: Doc
dot = text "."

class Pretty a where
  showPretty :: a -> Reader Settings Doc

-- instance Pretty ModuleIds where
  -- showPretty (ModuleIds mids) =
    -- ttext $ Text.intercalate "." $ map unCon $ NE.toList mids

-- instance Pretty PackageImport where
  -- showPretty =
    -- maybe empty (doubleQuotes . ttext) . unPkgImport

-- instance Pretty SortedImportGroups where
  -- showPretty =
    -- vgroups . (fmap . fmap) showPretty . unGroup

-- instance Pretty ImportDecl where
  -- showPretty (ImportSimple x) =
    -- showPretty x
  -- showPretty (ImportSimpleAll x) =
    -- showPretty x <+> text "()"
  -- showPretty (ImportAs x) =
    -- showPretty x
  -- showPretty (ImportSpec x) =
    -- showPretty x
  -- showPretty (ImportHidingSpec x) =
    -- showPretty x
  -- showPretty (ImportAsSpec x) =
    -- showPretty x
  -- showPretty (ImportAsHidingSpec x) =
    -- showPretty x

importText :: Doc
importText =
  text "import"

qualifiedText :: Doc
qualifiedText =
  text "qualified"

asText :: Doc
asText =
  text "as"

hidingText :: Doc
hidingText =
  text "hiding"

-- prettyPkg :: (HasPkg x PackageImport) => x -> Doc
-- prettyPkg =
  -- showPretty . view pkg

-- prettyModIds :: (HasModIds x ModuleIds) => x -> Doc
-- prettyModIds =
  -- showPretty . view modIds

-- prettyQualify :: (HasIsQual x Bool) => x -> Doc
-- prettyQualify  =
  -- bool empty qualifiedText . view isQual

-- prettyAs :: (HasAs x ModuleIds) => x -> Doc
-- prettyAs =
  -- showPretty . view as

-- prettySpecs :: (HasSpecs x SpecificImports) => x -> Doc
-- prettySpecs =
  -- showPretty . view specs

-- prettyHiding :: (HasHiding x SpecificImports) => x -> Doc
-- prettyHiding =
  -- showPretty . view hiding

-- instance Pretty ImportSimpleData where
  -- showPretty x =
    -- hsep
      -- [ importText,
        -- prettyPkg x,
        -- prettyModIds x,
        -- prettyQualify x
      -- ]

-- instance Pretty ImportAsData where
  -- showPretty x =
    -- hsep
      -- [ importText,
        -- prettyPkg x,
        -- prettyModIds x,
        -- prettyQualify x,
        -- asText,
        -- prettyAs x
      -- ]

-- instance Pretty ImportSpecData where
  -- showPretty x =
    -- hang
      -- ( hsep
          -- [ importText,
            -- prettyPkg x,
            -- prettyModIds x,
            -- prettyQualify x
          -- ]
      -- )
      -- 2
      -- (prettySpecs x)

-- instance Pretty ImportHidingSpecData where
  -- showPretty x =
    -- hang
      -- ( hsep
          -- [ importText,
            -- prettyPkg x,
            -- prettyModIds x,
            -- prettyQualify x,
            -- hidingText
          -- ]
      -- )
      -- 2
      -- (prettyHiding x)

-- instance Pretty ImportAsSpecData where
  -- showPretty x =
    -- hang
      -- ( hsep
          -- [ importText,
            -- prettyPkg x,
            -- prettyModIds x,
            -- prettyQualify x,
            -- asText,
            -- prettyAs x
          -- ]
      -- )
      -- 2
      -- (prettySpecs x)

-- instance Pretty ImportAsHidingSpecData where
  -- showPretty x =
    -- hang
      -- ( hsep
          -- [ importText,
            -- prettyPkg x,
            -- prettyModIds x,
            -- prettyQualify x,
            -- asText,
            -- prettyAs x,
            -- hidingText
          -- ]
      -- )
      -- 2
      -- (prettyHiding x)

-- instance Pretty SpecificImports where
  -- showPretty (SpecificImports (x :| [])) =
    -- parens $ showPretty x
  -- showPretty (SpecificImports imports) =
    -- layoutVerticalImports imports

-- layoutVerticalImports :: (Pretty a) => NonEmpty a -> Doc
-- layoutVerticalImports (x :| []) =
  -- parens $ showPretty x
-- layoutVerticalImports (x :| xs) =
  -- vcat
    -- [ lparen <+> showPretty x,
      -- vcat $ map ((comma <+>) . showPretty) xs,
      -- rparen
    -- ]

-- instance Pretty Import where
  -- showPretty (ImportVar x) =
    -- showPretty x
  -- showPretty (ImportVarSym x) =
    -- showPretty x
  -- showPretty (ImportTyCon x) =
    -- showPretty x
  -- showPretty (ImportTyConAll x) =
    -- showPretty x <+> "(..)"
  -- showPretty (ImportTyConCNames tycon c) =
    -- hang (showPretty tycon) 0 (layoutVerticalImports $ unCNames c)

-- | Inspired by https://hackage.haskell.org/package/ghc-lib-parser-8.10.1.20200324/docs/GHC-Hs-ImpExp.html#t:ImportDecl
-- data ImportDecl = ImportDecl
 -- { ideclName :: Qual
 -- -- ^ A ModuleName is essentially a string e.g. Data.List
 -- , ideclPkgQual :: Maybe Text
 -- -- ^ Package qualifier
 -- , ideclIsQual :: Bool
 -- -- ^ Does the qualified keyword appear
 -- , ideclAs :: Maybe Qual
 -- -- ^ as Module
 -- , ideclHiding :: Maybe (Bool, [IE])
 -- -- ^ (True => hiding, names)
 -- } deriving (Show, Eq)

prettyPkg :: ImportDecl -> Doc
prettyPkg =
   maybe empty (doubleQuotes . ttext) . ideclPkgQual

prettyName :: Settings -> ImportDecl -> Doc
prettyName settings x =
  case qualifiedStyle settings of
    QualifiedPre -> qualifiedText <+> modids
    QualifiedPost -> modids <+> qualifiedText
  where
    modids = runReader (showPretty $ ideclName x) settings

prettyHiding :: ImportDecl -> Doc
prettyHiding x = empty

instance Pretty ImportDecls where
  showPretty x = do
    settings <- ask
    pure . vcat . map (\decl -> runReader (showPretty decl) settings) $ unImportDecls x

instance Pretty ImportDecl where
  showPretty x = do
    settings <- ask

    pure $ hang
      ( hsep
          [ importText
          , prettyPkg x
          , prettyName settings x
            -- prettyModIds x,
            -- prettyQualify x,
            -- asText,
            -- prettyAs x,
            -- hidingText
          ]
      )
      2
      $ prettyHiding x


instance Pretty Pragma where
  showPretty (Pragma x) =
    pure $ text "{-# LANGUAGE" <+> ttext x <+> text "#-}"

instance Pretty GhcOption where
  showPretty (GhcOption x) =
    pure $ text "{-# OPTIONS_GHC" <+> ttext x <+> text "#-}"

instance Pretty Qual where
  showPretty (Qual conids cname) = do
    settings <- ask
    pure $
      hcat . intersperse dot $
      map (\c -> runReader (showPretty c) settings) conids
      ++ [runReader (showPretty cname) settings]

instance Pretty CName where
  showPretty (CVarId x) =
    asks $ runReader (showPretty x)
  showPretty (CVarSym x) =
    asks $ runReader (showPretty x)
  showPretty (CConId x) =
    asks $ runReader (showPretty x)
  showPretty (CConSym x) =
    asks $ runReader (showPretty x)

instance Pretty VarId where
  showPretty =
    pure . ttext . unVarId

instance Pretty VarSym where
  showPretty =
    pure . ttext . unVarSym

instance Pretty ConId where
  showPretty =
    pure . ttext . unConId

instance Pretty ConSym where
  showPretty =
    pure . ttext . unConSym

ex2 :: Doc
ex2 =
  nest 2 $
    vcat
      [ lparen <+> text "A",
        (hsep [comma, text "B"]),
        (hsep [comma, text "C"]),
        (hsep [comma, text "D"]),
        rparen
      ]

ex3 :: Doc
ex3 =
  vcat $ map (\x -> comma <+> text x) ["B", "C", "D", "E", "F"]
