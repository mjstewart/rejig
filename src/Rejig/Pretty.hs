module Rejig.Pretty where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.List as L
import Rejig.Ast
import Rejig.Lang
import Rejig.Settings
import Text.PrettyPrint
import Prelude hiding (empty)

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

borderLine :: Doc
borderLine =
  ttext $ T.replicate lineLength "-"
  where
    lineLength = 80

paddedBorderLine :: Doc
paddedBorderLine =
  newline $$ borderLine $$ newline

{-|
  import A.B as C
    ( hello      <- handles layout for this section
    , world
    )
-}
layoutVerticalIEs :: Settings -> Bool -> [IE] -> Doc
layoutVerticalIEs settings export = \case
  [] -> if export then empty else parens empty
  (x : xs) ->
    vcat
      [ lparen <+> runReader (showPretty x) settings
      , vcat $ map ((comma <+>) . runReader' settings . showPretty) xs
      , rparen
      ]

-- | surround package qualifier in quotes
prettyPkg :: ImportDecl -> Doc
prettyPkg =
  maybe empty (doubleQuotes . ttext) . ideclPkgQual

prettyName :: Settings -> ImportDecl -> Doc
prettyName settings x =
  if ideclIsQual x then
    case _ssrcLang settings of
      -- QualifiedPre
      Haskell -> qualifiedText <+> modids
      -- QualifiedPost
      Daml -> modids <+> qualifiedText
  else modids
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
      layoutVerticalIEs settings False ies

prettyAs :: Settings -> ImportDecl -> Doc
prettyAs settings =
  maybe empty ((asText <+>) . runReader' settings . showPretty) . ideclAs

-- | vcat but put newlines between
vcatSep:: [Doc] -> Doc
vcatSep =
  vcat . intersperse newline . filter (not . isEmpty)

hasImports :: SortedModuleHeader -> Bool
hasImports mh =
  rest || prefixes || pkgQuals
  where
    pi = _smodImports mh
    rest = not . null . unImportDeclGroups . _cgGroup $ _piRest pi
    prefixes = not . null $ _piPrefixTargets pi
    pkgQuals = not . null . unImportDeclGroups . _cgGroup $ _piPkgQuals pi

hasImportsPs :: SortedParsedSource -> Bool
hasImportsPs =
  hasImports . _docThing . _ssrcModHeader

instance Pretty SortedParsedSource where
  showPretty x = do
    settings <- ask

    pure $ vcatSep
     [ prettyVcat settings $ _ssrcGhcOpts x
     , prettyVcat settings $ _ssrcLangExts x
     , runReader' settings . showPretty $ _ssrcModHeader x
     , if hasImportsPs x && _sImportBorderBottom settings then borderLine else empty
     , ttext $ _ssrcRest x
     ]

{-| When printing comments, we need to preserve multiline comments while still
  identifiying single and block comments.

  The parser sets up the structure to make our life easier.

  [SingleLineComment, SingleLineComment, CommentNewLine, BlockComment]

  The `CommentNewLine` constructor is a flag to represent the end of comment
  group where we insert a new line between.
-}
toCommentGroups :: Settings -> [Comment] -> Doc
toCommentGroups settings =
  vcatSep
    . map (vcat . map (runReader' settings . showPretty))
    . L.groupBy f
  where
    f (SingleLineComment _) (SingleLineComment _) = True
    f (BlockComment _) (BlockComment _) = True
    f _ _ = False

instance (Pretty a) => Pretty (DocString a) where
  showPretty x = do
    settings <- ask

    -- comment sticks to its next top level decl
    pure $
      vcat [
        toCommentGroups settings $ _docComments x
      , runReader' settings . showPretty $ _docThing x
      ]

prettyVcat :: Pretty a => Settings -> [a] -> Doc
prettyVcat s = vcat . map (runReader' s . showPretty)

instance Pretty Comment where
  showPretty = \case
    SingleLineComment x -> pure $ text "--" <+> (ttext $ T.strip x)
    CommentNewLine -> pure empty
    BlockComment x ->
      -- some special handling for haddock block comment so no white space is added when joining
      pure $ vcat [
        if T.isPrefixOf "|" $ T.strip x
        then hcat [text "{-", ttext $ T.strip x]
        else text "{-" <+> (ttext $ T.strip x)
       , text "-}"
      ]

instance Pretty SortedModuleHeader where
  showPretty x = do
    settings <- ask

    pure $
      vcat
        [ hang
          ( hsep
              [ text "module"
              , runReader' settings . showPretty $ _smodName x
              ]
          )
          2
          $ layoutVerticalIEs settings True $ _smodExports x
        , text "where"
        , if hasImports x then
            if _sImportBorderTop settings then newline $$ borderLine $$ newline else newline
          else empty
        , runReader' settings . showPretty $ _smodImports x
        ]

{-| Layout structure for import decls

  1. standard imports
  2. imports grouped by user defined targeted prefixes
  3. package qualified imports
-}
instance Pretty PartitionedImports where
  showPretty x = do

    liftA3
      (\a b c -> vcatSep [a, b, c])
      (prettyCG $ _piRest x)
      (vcatSep <$> (mapM prettyCG $ _piPrefixTargets x))
      (prettyCG $ _piPkgQuals x)

{-| An optional title is included if the setting is enabled

  -- comment group title

  import decl 1
  import decl 2
  ...
-}
prettyCG :: Pretty a => CG a -> Reader Settings Doc
prettyCG cg = do
  settings <- ask

  let
    content = runReader' settings $ showPretty $ _cgGroup cg

    comment =
      if _sDisplayGroupTitle settings then
        maybe empty (\c -> vcat [singleLineComment c, newline]) $ _cgComment cg
      else empty

  pure $
    if isEmpty content then empty
    else
      vcat
        [ comment
        , content
        ]

instance Pretty ImportDeclGroups where
  showPretty x = do
    settings <- ask
    pure . vcatSep . map (runReader' settings . showPretty) $ unImportDeclGroups x

instance Pretty ImportDecls where
  showPretty x = do
    settings <- ask
    pure . vcat . map (runReader' settings . showPretty) $ unImportDecls x

instance Pretty ImportDecl where
  showPretty x = do
    settings <- ask

    pure $
      hang
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
    asks $ runReader (showPretty x)
  showPretty (IEThingAll x) =
    asks $ \settings -> runReader (showPretty x) settings <+> parens (text "..")
  showPretty (IEThingWith x xs) =
    asks $ \settings ->
      hsep
        [ runReader (showPretty x) settings
        , parens $ hsep $ punctuate comma $ map (runReader' settings . showPretty) xs
        ]
  showPretty (IEModuleContents x) =
    asks $ \settings ->
      hsep
        [ text "module"
        , runReader (showPretty x) settings
        ]

instance Pretty QVar where
  showPretty (VId x) =
    asks $ runReader (showPretty x)
  showPretty (VSym x) =
    asks $ runReader (showPretty x)

instance Pretty LangExt where
  showPretty (LangExt x) =
    pure $ text "{-# LANGUAGE" <+> ttext x <+> text "#-}"

instance Pretty GhcOption where
  showPretty (GhcOption x) =
    pure $ text "{-# OPTIONS_GHC" <+> ttext x <+> text "#-}"

-- | A.B.C.thing
instance Pretty Qual where
  showPretty (Qual conids thing) = do
    settings <- ask
    pure $
      hcat $
        intersperse dot $
          map (runReader' settings . showPretty) conids ++ [ttext thing]

instance Pretty Text where
  showPretty = pure . ttext

instance Pretty CName where
  showPretty (CVarId x) =
    asks $ runReader (showPretty x)
  showPretty (CVarSym x) =
    asks $ runReader (showPretty x)
  showPretty (CConId x) =
    asks $ runReader (showPretty x)
  showPretty (CConSym x) =
    asks $ runReader (showPretty x)

instance Pretty QVarId where
  showPretty (QVarId x)=
    asks $ runReader (showPretty x)

instance Pretty QVarSym where
  showPretty (QVarSym x) =
    asks $ runReader (showPretty x)

instance Pretty QConId where
  showPretty (QConId x) =
    asks $ runReader (showPretty x)

instance Pretty QConSym where
  showPretty (QConSym x) =
    asks $ runReader (showPretty x)

instance Pretty ConId where
  showPretty =
    pure . ttext . unConId
