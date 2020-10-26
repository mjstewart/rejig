module Rejig.Pretty
where

--------------------------------------------------------------------------------

-- standard imports

import Control.Monad.Reader
import Text.PrettyPrint

import Prelude hiding
  ( empty
  )

import qualified Data.List as L
import qualified Data.Text as T

-- imports by Rejig*

import Rejig.Ast
import Rejig.Lang
import Rejig.Settings

--------------------------------------------------------------------------------

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

layoutVertically :: [Doc] -> Doc
layoutVertically = \case
  [] -> empty
  (x : xs) ->
    vcat
      [ lparen <+> x
      , vcat $ map ((comma <+>)) xs
      , rparen
      ]

runPrettyReader' :: Pretty a => Settings -> a -> Doc
runPrettyReader' settings =
  runReader' settings . showPretty

runPrettyReader :: Pretty a => a -> Settings -> Doc
runPrettyReader =
  flip runPrettyReader'

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
    modids = runPrettyReader' settings $ ideclName x

preHiding :: ImportDecl -> Doc
preHiding x =
  maybe empty (\(isHiding, _) -> bool empty hidingText isHiding) $ ideclHiding x

prettyHiding :: Settings -> ImportDecl -> Doc
prettyHiding settings x =
  case ideclHiding x of
    Nothing -> empty
    Just (_, ies) ->
      layout $ map (runPrettyReader' settings) ies
  where
    layout = \case
      [] -> parens empty
      xs -> layoutVertically xs

prettyAs :: Settings -> ImportDecl -> Doc
prettyAs settings =
  maybe empty ((asText <+>) . runPrettyReader' settings) . ideclAs

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

prettyVcat :: Pretty a => Settings -> [a] -> Doc
prettyVcat s =
  vcat . map (runPrettyReader' s)

{-| When printing comments, we need to preserve multiline comments while still
  identifying single and block comments so they aren't all mashed together.

  The parser sets up a convenient structure, the prettifier just does some logic to work out
  the spaces to identify the groups.

  [SingleLineComment, SingleLineComment, CommentNewLine, BlockComment]

  The `CommentNewLine` constructor is a flag to represent the end of comment
  group where we insert a new line between. Many new lines are collapsed into 1 and
  ending newlines are respected to retain floating comments not directly tied to a top level decl.
-}
toCommentGroups :: Settings -> [Comment] -> Doc
toCommentGroups settings cs =
  vcatSep
    (map (vcat . map (runPrettyReader' settings)) $ L.groupBy f cs)
    $$ ending
  where
    f (SingleLineComment _) (SingleLineComment _) = True
    f (BlockComment _) (BlockComment _) = True
    f _ _ = False

    ending =
      case listToMaybe $ reverse cs of
        Nothing -> empty
        Just CommentNewLine -> newline
        _ -> empty

instance Pretty SortedParsedSource where
  showPretty x = do
    settings <- ask

    pure $
      vcat [
        toCommentGroups settings $ _ssrcInitialDocs x
      , vcatSep
          [ prettyVcat settings $ _ssrcGhcOpts x
          , prettyVcat settings $ _ssrcLangExts x
          , runPrettyReader' settings $ _ssrcModHeader x
          , if hasImportsPs x && _sImportBorderBottom settings then borderLine else empty
          , ttext $ _ssrcRest x
          ]
      ]

instance (Pretty a) => Pretty (DocString a) where
  showPretty x = do
    settings <- ask

    pure $
      vcat [
        toCommentGroups settings $ _docComments x
      , runPrettyReader' settings $ _docThing x
      ]

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
              , runPrettyReader' settings $ _smodName x
              ]
          )
          2
          $ case _smodExports x of
              [] -> empty
              xs -> layoutVertically $ map (runPrettyReader' settings) xs
        , text "where"
        , if hasImports x then
            if _sImportBorderTop settings then newline $$ borderLine $$ newline else newline
          else empty
        , runPrettyReader' settings $ _smodImports x
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
    content = runPrettyReader' settings $ _cgGroup cg

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
    pure . vcatSep . map (runPrettyReader' settings) $ unImportDeclGroups x

instance Pretty ImportDecls where
  showPretty x = do
    settings <- ask
    pure . vcat . map (runPrettyReader' settings) $ unImportDecls x

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
    asks $ runPrettyReader x
  showPretty (IEThingAbs x) =
    asks $ runPrettyReader x
  showPretty (IEThingAll x) =
    asks $ \settings -> runPrettyReader' settings x <+> parens (text "..")
  showPretty (IEThingWith x xs) =
    asks $ \settings ->
      hang
        (runPrettyReader' settings x)
        2
        $ layoutVertically $ map (runPrettyReader' settings) xs
  showPretty (IEModuleContents x) =
    asks $ \settings ->
      hsep
        [ text "module"
        , runPrettyReader' settings x
        ]
  showPretty (IEPatternContents x) =
    asks $ \settings ->
      hsep
        [ text "pattern"
        , runPrettyReader' settings x
        ]

instance Pretty QVar where
  showPretty (VId x) =
    asks $ runPrettyReader x
  showPretty (VSym x) =
    asks $ runPrettyReader x

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
          map (runPrettyReader' settings) conids ++ [ttext thing]

instance Pretty Text where
  showPretty = pure . ttext

instance Pretty CName where
  showPretty (CVarId x) =
    asks $ runPrettyReader x
  showPretty (CVarSym x) =
    asks $ runPrettyReader x
  showPretty (CConId x) =
    asks $ runPrettyReader x
  showPretty (CConSym x) =
    asks $ runPrettyReader x

instance Pretty QVarId where
  showPretty (QVarId x)=
    asks $ runPrettyReader x

instance Pretty QVarSym where
  showPretty (QVarSym x) =
    asks $ runPrettyReader x

instance Pretty QConId where
  showPretty (QConId x) =
    asks $ runPrettyReader x

instance Pretty QConSym where
  showPretty (QConSym x) =
    asks $ runPrettyReader x

instance Pretty ConId where
  showPretty =
    pure . ttext . unConId
