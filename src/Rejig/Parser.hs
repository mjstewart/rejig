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
import Rejig.Lexer
import Rejig.Pretty (Pretty, showPretty)
import Rejig.Settings
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (many, some)

-- | {-# LANGUAGE <Extension> #-}
pragmaP :: Parser Pragma
pragmaP =
  Pragma . T.pack
    <$> (p "{-#" *> keyword "language" *> manyTill letterChar (sc *> p "#-}"))
  where
    p = lexeme . string

-- {-# OPTIONS_GHC -Wno-all-case #-}
ghcOptionP :: Parser GhcOption
ghcOptionP =
  GhcOption . T.pack
    <$> (p "{-#" *> keyword "options_ghc" *> manyTill (letterChar <|> char '-') (sc *> p "#-}"))
  where
    p = lexeme . string

-- | The things in an import/export body
ieP :: Parser [IE]
ieP =
  parens $
    choice
      [ try thingWithP,
        try thingAllP,
        try thingAbsP,
        varP
      ]
      `sepBy` comma
  where
    varP = try (VId <$> varid) <|> (VSym <$> varsym) <&> IEVar

    thingAbsP = IEThingAbs <$> conid

    thingAllP = IEThingAll <$> conid <* parens (dot >> dot)

    thingWithP = IEThingWith <$> conid <*> cnameP

    cnameP =
      parens $
        choice
          [ try $ CVarId <$> varid,
            try $ CVarSym <$> varsym,
            try $ CConId <$> conid,
            try $ CConSym <$> consym
          ]
          `sepBy` comma

importDeclP :: Parser ImportDecl
importDeclP = do
  void $ keyword "import"
  ideclPkgQual <- optional stringLit
  (ideclIsQual, ideclName) <- ideclNameP
  ideclAs <- optional (keyword "as" *> qconid)
  ideclHiding <- ideclHidingP
  pure ImportDecl {..}
  where
    ideclNameP :: Parser (Bool, Qual)
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

-- exportsP :: arser
