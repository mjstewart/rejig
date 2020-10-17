{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Rejig.Parser where

import Control.Monad (void)
import Control.Monad.Combinators
import Control.Monad.Reader
import qualified Data.Char as Char
import qualified Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Void
import Rejig.Ast
import Rejig.Lang
import Text.Megaparsec.Debug
import Rejig.Lexer
import Rejig.Pretty (Pretty, showPretty)
import Rejig.Settings
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (many, some)

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
      [ try thingWithP,
        try thingAllP,
        try thingAbsP,
        try moduleContents,
        varP
      ]
      `sepBy` comma
  where
    varP = try (VId <$> qvarid) <|> (VSym <$> qvarsym) <&> IEVar

    thingAbsP = IEThingAbs <$> qconid

    thingAllP = IEThingAll <$> qconid <* parens (dot >> dot)

    thingWithP = IEThingWith <$> qconid <*> cnameP

    moduleContents = IEModuleContents <$> (keyword "module" *> qconid)

    cnameP =
      parens $
        choice
          [ try $ CVarId <$> qvarid,
            try $ CVarSym <$> qvarsym,
            try $ CConId <$> qconid,
            try $ CConSym <$> qconsym
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
        [ try $ qual *> ((True,) <$> qconid),
          try $ ((True,) <$> qconid) <* qual,
          (False,) <$> qconid
        ]

    ideclHidingP :: Parser (Maybe (Bool, [IE]))
    ideclHidingP =
      optional $
        choice
          [ try (keyword "hiding") *> ((True,) <$> ieP),
            (False,) <$> ieP
          ]

    qual = keyword "qualified"

importsP :: Parser ImportDecls
importsP =
  ImportDecls <$> many importDeclP

modHeaderP :: Parser ModuleHeader
modHeaderP = do
  _modLangExts <- many $ try langExtP
  _modGhcOpts <- many $ try ghcOptionP
  _modName <- keyword "module" *> qconid
  _modExports <- ieP <* keyword "where"
  _modImports <- importsP
  pure $ ModuleHeader {..}

parseSourceP :: Parser ParsedSource
parseSourceP = do
  _srcModHeader <- leadingCommentedThingP modHeaderP
  _srcRest <- T.pack <$> (manyTill anySingle eof)
  pure $ ParsedSource {..}

leadingCommentedThingP :: Parser a -> Parser (LeadingCommentedThing a)
leadingCommentedThingP p = do
  LeadingCommentedThing <$> leadingCommentsP p <*> p

{-
   {- block -} | [-- sl] | module start
-}
leadingCommentsP :: Parser a -> Parser [Comment]
leadingCommentsP p =
  emptySc *> (many $
    choice [
      try $ SingleLineComment <$> singleLineCommentP
    , try $ CommentNewLine <$ (elexeme newline)
    , try $ blockCommentP
    ])

-- manySingleLines :: Parser a -> Parser (LeadingCommentedThing a)
-- manySingleLines p = do
  -- (a, b) <- (manyTill_ singleLineCommentP (leadingCommentsP p))
  -- let
    -- comments = SingleLineComments a

    -- restComments = _leadingComments b
    -- thing = _leadingThing b

  -- pure $ LeadingCommentedThing (comments : restComments) thing
  -- pure $ LeadingCommentedThing [(SingleLineComments a)] b


  -- many (try (elexeme singleLineCommentsP) <|> blockCommentP)
  -- many singleLineCommentsP
  -- emptySc *> (many $ (try singleLineCommentP <|> try blockCommentP))

-- m2 :: Parser Comment
-- m2 =
  -- SingleLineComments <$>
    -- (try (many (singleLineCommentP <* lookAhead singleLineCommentP)))

-- singleLineCommentP :: Parser Comment
-- singleLineCommentP =
  -- many


-- singleLineCommentsP :: Parser Comment
-- singleLineCommentsP =
  -- SingleLineComments <$> (manyTill (dbg "scp" singleLineCommentP) (dbg "lahead" $ lookAhead leadingCommentsP))

  -- many (try blockCommentP)
  -- SingleLineComments <$> (many singleLineCommentP)


  -- (x, y) <- manyTill_ (elexeme singleLineCommentP) (many langExtP)
  -- pure $ LeadingCommentedThing [SingleLineComments x] y
  -- SingleLineComments <$> (manyTill singleLineCommentP end)

-- singleLineComments2P :: Parser (LeadingCommentedThing [LangExt])
-- singleLineComments2P = do
  -- (x, y) <- manyTill_ (elexeme singleLineCommentP) ()
  -- pure $ LeadingCommentedThing [SingleLineComments x] y
