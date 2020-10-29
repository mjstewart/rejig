{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Rejig.Parser
where

--------------------------------------------------------------------------------

-- standard imports

import Control.Monad.Combinators
import Control.Monad.Reader
import Text.Megaparsec
import Text.Megaparsec.Char

import Prelude hiding
  ( many
  , some
  )

import qualified Data.Text as T

-- imports by Rejig*

import Rejig.Ast
import Rejig.Lexer

import Rejig.Lang
  ( Parser
  )
import qualified Data.Text.IO as TIO

--------------------------------------------------------------------------------

-- | {-# LANGUAGE <Extension> #-}
langExtP :: Parser LangExt
langExtP =
  LangExt . T.pack
    <$> (pragma $ lexeme (keyword "language" *> many letterChar))

-- {-# OPTIONS_GHC -Wno-all-case #-}
ghcOptionP :: Parser GhcOption
ghcOptionP =
  GhcOption . T.pack
    <$> (pragma $ lexeme (keyword "options_ghc" *> many (letterChar <|> char '-')))

-- | The things in an import/export body
ieP :: Parser [IE]
ieP =
  parens $
    choice
      [ try thingWithP
      , try thingAllP
      , try thingAbsP
      , try moduleContents
      , try patternContents
      , varP
      ]
      `sepBy` comma
  where
    varP = try (VId <$> qvarid) <|> (VSym <$> qvarsym) <&> IEVar

    thingAbsP = IEThingAbs <$> qconid

    thingAllP = IEThingAll <$> qconid <* parens (dot >> dot)

    thingWithP = IEThingWith <$> qconid <*> cnameP

    moduleContents = IEModuleContents <$> (keyword "module" *> qconid)

    patternContents = IEPatternContents <$> (keyword "pattern" *> qconid)

    cnameP =
      parens $
        choice
          [ try $ CVarId <$> qvarid
          , try $ CVarSym <$> qvarsym
          , try $ CConId <$> qconid
          , try $ CConSym <$> qconsym
          ]
          `sepBy` comma

importDeclP :: Parser ImportDecl
importDeclP = do
  void $ keyword "import"
  ideclPkgQual <- optional stringLit
  (ideclIsQual, ideclName) <- ideclNameP
  ideclAs <- optional $ keyword "as" *> qconid
  ideclHiding <- ideclHidingP
  pure ImportDecl {..}
  where
    ideclNameP :: Parser (Bool, QConId)
    ideclNameP =
      choice
        [ try $ qual *> ((True,) <$> qconid)
        , try $ ((True,) <$> qconid) <* qual
        , (False,) <$> qconid
        ]

    ideclHidingP :: Parser (Maybe (Bool, [IE]))
    ideclHidingP =
      optional $
        choice
          [ try (keyword "hiding") *> ((True,) <$> ieP)
          , (False,) <$> ieP
          ]

    qual = keyword "qualified"

importsP :: Parser ImportDecls
importsP =
  ImportDecls <$> many (rejigLexeme importDeclP)

modHeaderP :: Parser ModuleHeader
modHeaderP = do
  _modName <- keyword "module" *> qconid
  _modExports <- (ieP <|> pure []) <* rejigLexeme (keyword "where")
  _modImports <- importsP
  pure $ ModuleHeader {..}

parseSourceP :: Parser ParsedSource
parseSourceP = do
  _srcInitialDocs <- leadingCommentsP
  _srcGhcOpts <- many $ try $ docsP ghcOptionP
  _srcLangExts <- many $ try $ docsP langExtP
  _srcModHeader <- docsP modHeaderP
  _srcRest <- T.pack <$> everythingElse
  pure $ ParsedSource {..}
  where
    everythingElse = manyTill anySingle eof

t2 :: Parser (DocString ModuleHeader)
t2 =
  docsP modHeaderP

-- | Collects any comments that occur before `p`.
docsP :: Parser a -> Parser (DocString a)
docsP p = do
  DocString <$> leadingCommentsP <*> p

leadingCommentsP :: Parser [Comment]
leadingCommentsP =
  scn *> (many $
    choice [
      try $ SingleLineComment <$> singleLineCommentP
    , try $ CommentNewLine <$ emptyLineP
    , try $ blockCommentP
    ])

parseFile2 :: FilePath -> IO ()
parseFile2 path = do
  txt <- TIO.readFile path

  -- case runParser leadingCommentsP "test" txt of
  case runParser parseSourceP "test" txt of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right res ->
      putStrLn $ show res
