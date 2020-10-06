{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Rejig.Parser where

import Text.Megaparsec
import qualified Data.Set as Set
import qualified Data.Text as Text

import Control.Monad (void)
import Control.Monad.Combinators
import Rejig.Lang
import Rejig.Lexer
import Rejig.Ast
import Rejig.Pretty (showPretty, Pretty)
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (many, some)

import qualified Data.Text as T
import Rejig.Settings
import Control.Monad.Reader


-- | {-# LANGUAGE <Extension> #-}
pragmaP :: Parser Pragma
pragmaP =
  Pragma . T.pack <$>
    (p "{-#" *> keyword "language" *> manyTill letterChar (sc *> p "#-}"))
  where
    p = lexeme . string

-- {-# OPTIONS_GHC -Wno-all-case #-}
ghcOptionP :: Parser GhcOption
ghcOptionP =
  GhcOption . T.pack <$>
    (p "{-#" *> keyword "options_ghc" *> manyTill (letterChar <|> char '-') (sc *> p "#-}"))
  where
    p = lexeme . string

-- | The things in an import/export body
ieP :: Parser [IE]
ieP =
  parens $ choice
   [ try thingWithP
   , try thingAllP
   , try thingAbsP
   , varP
   ]
  `sepBy` comma

  where
    varP = try (VId <$> varid) <|> (VSym <$> varsym) <&> IEVar

    thingAbsP = IEThingAbs <$> conid

    thingAllP = IEThingAll <$> conid <* parens (dot >> dot)

    thingWithP = IEThingWith <$> conid <*> cnameP

    cnameP = parens $ choice
     [ try $ CVarId <$> varid
     , try $ CVarSym <$> varsym
     , try $ CConId <$> conid
     , try $ CConSym <$> consym
     ] `sepBy` comma

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
    ideclNameP = choice
      [ try $ qual *> ((True,) <$> qconid)
      , try $ ((True,) <$> qconid) <* qual
      , (False,) <$> qconid
      ]

    ideclHidingP :: Parser (Maybe (Bool, [IE]))
    ideclHidingP =
      optional $
        choice
          [ try (keyword "hiding") *> ((True, ) <$> ieP)
          , (False,) <$> ieP
          ]

    qual = keyword "qualified"


importsP :: Parser ImportDecls
importsP =
  ImportDecls <$> many importDeclP
