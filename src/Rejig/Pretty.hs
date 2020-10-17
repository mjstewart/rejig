module Rejig.Pretty where

import Control.Monad.Reader
import qualified Data.Set as Set
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

layoutVerticalIEs :: Settings -> [IE] -> Doc
layoutVerticalIEs settings = \case
  [] -> parens empty
  [x] -> parens $ runReader (showPretty x) settings
  (x : xs) ->
    vcat
      [ lparen <+> runReader (showPretty x) settings,
        vcat $ map ((comma <+>) . runReader' settings . showPretty) xs,
        rparen
      ]

prettyPkg :: ImportDecl -> Doc
prettyPkg =
  maybe empty (doubleQuotes . ttext) . ideclPkgQual

prettyName :: Settings -> ImportDecl -> Doc
prettyName settings x =
  if ideclIsQual x
    then case _sQualifiedStyle settings of
      QualifiedPre -> qualifiedText <+> modids
      QualifiedPost -> modids <+> qualifiedText
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
      layoutVerticalIEs settings ies

prettyAs :: Settings -> ImportDecl -> Doc
prettyAs settings =
  maybe empty ((asText <+>) . runReader' settings . showPretty) . ideclAs

instance Pretty Comment where
  showPretty = \case
    -- SingleLineComments xs -> pure $ vcat $ map (\x -> text "--" <+> (ttext $ T.strip x)) xs
    SingleLineComment x -> pure $ text "--" <+> (ttext $ T.strip x)
    CommentNewLine -> pure empty
    BlockComment x ->
      pure $ vcat [
        if T.isPrefixOf "|" (T.strip x) then
          hcat [text "{-", ttext $ T.strip x]
        else
          text "{-" <+> (ttext $ T.strip x)
      ,
       text "-}"
      ]

-- vcatGroup :: [a] -> Doc
-- vcatGroup xs =
  -- if null xs  else [vcat xs, newline]

vcatSep:: [Doc] -> Doc
vcatSep =
  vcat . intersperse newline . filter (not . isEmpty)


instance Pretty ParsedSource where
  showPretty x = do
    settings <- ask

    pure $ vcatSep
     [ runReader' settings . showPretty $ _srcModHeader x
     , ttext $ _srcRest x
     ]
    -- _srcModHeader


-- | Groups exists to make formatting easier since each type of comment
-- style is separated by a newline whilst multiline comments are preserved.
toCommentGroups :: Settings -> [Comment] -> Doc
toCommentGroups settings =
  vcatSep
    . map (vcat . map (runReader' settings . showPretty))
    . L.groupBy f
  where
    f (SingleLineComment _) (SingleLineComment _) = True
    f (BlockComment _) (BlockComment _) = True
    f _ _ = False

instance (Pretty a) => Pretty (LeadingCommentedThing a) where
  showPretty x = do
    settings <- ask

    pure $
      vcatSep [
        -- ttext $ show $ _leadingComments x
      -- , ttext $ show $ g $ _leadingComments x
      -- vcat $ map (runReader' settings . showPretty) $ pt3 settings (g (_leadingComments x))
       toCommentGroups settings $ _leadingComments x
      , runReader' settings . showPretty $ _leadingThing x
      ]


prettyVcat :: Pretty a => Settings -> [a] -> Doc
prettyVcat s = vcat . map (runReader' s . showPretty)


instance Pretty ModuleHeader where
  showPretty x = do
    settings <- ask

    pure $ vcatSep [
        prettyVcat settings $ _modLangExts x
      , prettyVcat settings $ _modGhcOpts x
        -- vcat $ map (runReader' settings . showPretty) $ _modLangExts x
      -- , vcat $ map (runReader' settings . showPretty) $ _modGhcOpts x
      ,
      vcat
       [ hang
        ( hsep
            [ text "module"
            , runReader' settings . showPretty $ _modName x
            ]
        )
        2
        $ layoutVerticalIEs settings $ _modExports x
       , text "where"
       ]
      , runReader' settings . showPretty $ _modImports x
      ]

t2 :: Doc
t2 =
  hang (hsep [(text "me"), text "you"])
   2
   $ vcat [text "a", text "b", text "c"]
    -- pure $ text "module header"

x :: IO ()
x =
  writeFile "result.txt" $ render t2


instance Pretty PartitionedImports where
  showPretty x = do

    liftA3
      (\a b c -> vcat [a, b, c])
      (prettyCG $ _piRest x)
      (vcat <$> (mapM prettyCG $ _piPrefixTargets x))
      (prettyCG $ _piPkgQuals x)

-- <*> (prettyCG $ _piPrefixTargets x)
-- <*> (mapM prettyCG $ _piPkgQuals x )

-- pure . vcat . intersperse newline . map (runReader' settings . showPretty) $ unImportDeclGroups x

prettyCG :: Pretty a => CG a -> Reader Settings Doc
prettyCG cg = do
  settings <- ask
  pure $
    vcat
      [ comment settings,
        runReader' settings $ showPretty $ _cgGroup cg
      ]
  where
    comment settings =
      if _sShowGroupComments settings
        then maybe empty (\c -> vcat [newline, singleLineComment c, newline]) $ _cgComment cg
        else empty

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
            [ importText,
              prettyPkg x,
              prettyName settings x,
              prettyAs settings x,
              preHiding x
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
        [ runReader (showPretty x) settings,
          parens $ hsep $ punctuate comma $ map (runReader' settings . showPretty) xs
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

instance Pretty Qual where
  showPretty (Qual conids thing) = do
    settings <- ask
    pure $
      hcat $
        intersperse dot $
          map (runReader' settings . showPretty) conids ++ [ttext thing]

        -- map (runReader' settings . showPretty) conids
          -- ++ [runReader (showPretty thing) settings]

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
    --runReader showPretty . unQVarId
    -- pure . text . unQVarId

instance Pretty QVarSym where
  showPretty (QVarSym x) =
    asks $ runReader (showPretty x)
    -- pure . ttext . unQVarSym

instance Pretty QConId where
  showPretty (QConId x) =
    asks $ runReader (showPretty x)
  -- pure . ttext . unQConId

instance Pretty QConSym where
  showPretty (QConSym x) =
    asks $ runReader (showPretty x)

    -- pure . ttext . unQConSym

instance Pretty ConId where
  showPretty =
    pure . ttext . unConId
