module Rejig.Pretty where

import Rejig.Ast
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.PrettyPrint
import Prelude hiding (empty)
import Control.Monad.Reader
import Rejig.Settings
import Rejig.Lang


class Pretty a where
  showPretty :: a -> Reader Settings Doc

newline :: Doc
newline =
  text ""

ttext :: Text -> Doc
ttext = text . T.unpack

dot :: Doc
dot = text "."

importText :: Doc
importText =
  text "import"

singleLineComment :: Text -> Doc
singleLineComment =
  (text "--" <+>) . ttext

qualifiedText :: Doc
qualifiedText =
  text "qualified"

asText :: Doc
asText =
  text "as"

hidingText :: Doc
hidingText =
  text "hiding"

layoutVerticalImports :: Settings -> [IE] -> Doc
layoutVerticalImports settings = \case
  [] -> parens empty
  [x] -> parens $ runReader (showPretty x) settings
  (x:xs) ->
    vcat
      [ lparen <+> runReader (showPretty x) settings
        , vcat $ map ((comma <+>) . runReader' settings . showPretty) xs
        , rparen
      ]

prettyPkg :: ImportDecl -> Doc
prettyPkg =
   maybe empty (doubleQuotes . ttext) . ideclPkgQual

prettyName :: Settings -> ImportDecl -> Doc
prettyName settings x =
  if ideclIsQual x then
    case qualifiedStyle settings of
      QualifiedPre -> qualifiedText <+> modids
      QualifiedPost -> modids <+> qualifiedText
  else
    modids
  where
    modids = runReader' settings . showPretty $ ideclName x

preHiding :: ImportDecl -> Doc
preHiding x =
  maybe empty (\(isHiding, _) -> bool empty hidingText isHiding) $ ideclHiding x

prettyHiding :: Settings -> ImportDecl -> Doc
prettyHiding settings x =
  case ideclHiding x of
    Nothing -> empty
    Just (_, ies) ->
      layoutVerticalImports settings ies

prettyAs :: Settings -> ImportDecl -> Doc
prettyAs settings =
  maybe empty ((asText <+>) . runReader' settings . showPretty) . ideclAs

instance Pretty PartitionedImports where
  showPretty x = do
    settings <- ask

    vcat <$> (mapM prettyCG $ _piPrefixTargetGroups x)
    -- pure . vcat . intersperse newline . map (runReader' settings . showPretty) $ unImportDeclGroups x

prettyCG :: Pretty a => CG a -> Reader Settings Doc
prettyCG cg = do
  settings <- ask
  pure $ vcat
   [ comment
   , runReader' settings $ showPretty $ _cgGroup cg
   ]
  where
    comment = maybe empty (\c -> vcat [newline, singleLineComment c, newline]) $  _cgComment cg


instance Pretty ImportDeclGroups where
  showPretty x = do
    settings <- ask
    pure . vcat . intersperse newline . map (runReader' settings . showPretty) $ unImportDeclGroups x

instance Pretty ImportDecls where
  showPretty x = do
    settings <- ask
    pure . vcat . map (runReader' settings . showPretty) $ unImportDecls x

instance Pretty ImportDecl where
  showPretty x = do
    settings <- ask

    pure $ hang
      ( hsep
          [ importText
          , prettyPkg x
          , prettyName settings x
          , prettyAs settings x
          , preHiding x
          ]
      )
      2
      $ prettyHiding settings x

instance Pretty IE where
  showPretty (IEVar x) =
    asks $ runReader (showPretty x)
  showPretty (IEThingAbs x) =
    asks $ \settings -> runReader (showPretty x) settings <+> parens empty
  showPretty (IEThingAll x) =
    asks $ \settings -> runReader (showPretty x) settings <+> parens (text "..")
  showPretty (IEThingWith x xs) =
    asks $ \settings ->
      hsep
        [ runReader (showPretty x) settings
        , parens $ hsep $ punctuate comma $ map (runReader' settings . showPretty) xs
        ]

instance Pretty Var where
  showPretty (VId x) =
    asks $ runReader (showPretty x)
  showPretty (VSym x) =
    asks $ runReader (showPretty x)

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
      map (runReader' settings . showPretty) conids
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
