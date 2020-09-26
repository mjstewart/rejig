module Rejig.Lexer where

import Control.Monad (void)
import Control.Monad.Combinators
import Rejig.Lang
import Rejig.Ast
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Prelude hiding (many, some)
import Text.Megaparsec.Debug

special :: Set Char
special =
  Set.fromList
    [ '(',
      ')',
      ',',
      ';',
      '[',
      ']',
      '`',
      '{',
      '}'
    ]

reservedId :: Set Text
reservedId =
  Set.fromList
    [ "case",
      "class",
      "data",
      "default",
      "deriving",
      "do",
      "else",
      "if",
      "import",
      "in",
      "infix",
      "infixl",
      "infixr",
      "instance",
      "let",
      "module",
      "newtype",
      "of",
      "then",
      "type",
      "where",
      "_",
      "template"
    ]

reservedOp :: Set Text
reservedOp =
  Set.fromList
    [ "..",
      ":",
      "::",
      "=",
      "\\",
      "|",
      "<-",
      "->",
      "@",
      "~",
      "=>"
    ]

ascSymbols :: Set Char
ascSymbols =
  Set.fromList [
    '!',
    '#',
    '$',
    '%',
    '&',
    '*',
    '+',
    '.',
    '/',
    '<',
    '=',
    '>',
    '?',
    '@',
    '\\',
    '^',
    '|',
    '-',
    '~'
  ]

lineComment :: Parser ()
lineComment =
  L.skipLineComment "--"

blockComment :: Parser ()
blockComment =
  L.skipBlockComment "{-" "-}"

-- | space consumer including new lines
scn :: Parser ()
scn =
  L.space space1 lineComment blockComment

-- | literal space only consumer, no new lines
sc :: Parser ()
sc =
  L.space (void $ takeWhile1P Nothing (== ' ')) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: Text -> Parser Text
symbol = L.symbol scn

comma :: Parser ()
comma =
  void $ symbol ","

dot :: Parser ()
dot =
  void $ symbol "."

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

keyword :: Text -> Parser ()
keyword kw =
  void $ lexeme $ string' kw <* notFollowedBy alphaNumChar

stringLit :: Parser Text
stringLit =
  lexeme $ char '"' >> T.pack <$> manyTill L.charLiteral (char '"')


small :: Parser Char
small =
  lowerChar <|> char '_'

large :: Parser Char
large =
  upperChar

ascSymbol :: Parser Char
ascSymbol =
  postValidate check anySingle
  where
    check result =
      if Set.member result ascSymbols
      then Right result
      else Left $ show result <> " is not a legal ascii symbol"


wrap :: Text -> Text
wrap x = "(" <> x <> ")"

-- | ascSymbol | uniSymbol<special | _ | : | " | '>
-- where uniSymbol = any Unicode symbol or punctuation
sym :: Parser Char
sym =
  postValidate check parser
  where
    parser = ascSymbol <|> symbolChar <|> punctuationChar
    specialSyms = Set.union special $ Set.fromList ['_', ':', '\"', '\'']

    check result =
      if Set.member result specialSyms
      then Left $ show result <> " is a reserved special symbol"
      else Right result


-- | (small {small | large | digit | ' })<reservedid>
varid :: Parser VarId
varid =
  VarId <$> postValidate check parser
  where
    parser = lexeme $ T.cons <$> small <*> takeWhileP Nothing body

    body = applyOr [Char.isLower, Char.isUpper, Char.isDigit, (== '\'')]

    check result =
      if Set.member result reservedId
      then Left $ show result <> " is a reserved identifier"
      else Right result

-- | large {small | large | digit | ' }
conid :: Parser ConId
conid =
  ConId <$> (lexeme $ T.cons <$> large <*> takeWhileP Nothing body)
  where
    body = applyOr [Char.isLower, Char.isUpper, Char.isDigit, (== '\'')]


-- | ( symbol {symbol | :})<reservedop | dashes>
-- dashes ignored since lexeme/symbol space consumer ignores them
varsym :: Parser VarSym
varsym =
  VarSym <$> postValidate check parser
  where
    parser = lexeme $ wrap <$> (parens $ lexeme (T.cons <$> sym <*> body))
    body = T.pack <$> many (try sym) <|> show <$> char ':'

    check result =
      if Set.member result reservedOp
      then Left $ show result <> " is a reserved operator"
      else Right result

-- | (: {symbol | :})<reservedop>
consym :: Parser ConSym
consym =
  ConSym <$> postValidate check parser
  where
    parser = lexeme $ wrap <$> (parens $ lexeme (T.cons <$> char ':' <*> body))
    body = T.pack <$> (many $ try sym <|> char ':')

    check result =
      if Set.member result reservedOp
      then Left $ show result <> " is a reserved operator"
      else Right result

tyvar :: Parser VarId
tyvar = varid

tycon :: Parser ConId
tycon = conid

tycls :: Parser ConId
tycls = conid

modid :: Parser ConId
modid = conid

-- | This parser is abit involved due to the last element being a different lexeme type.
-- The lookAhead is a preprocessing step, similar to doing a string split to identify the last
-- element to run a different parser over
modids :: Parser CName -> Parser Qual
modids endP = do
  segmentCount <- (dec . length) <$> (lookAhead $ word `sepBy` dot)
  Qual <$> (count segmentCount $ modid <* dot) <*> endP
  where
    dec x = x - 1

    word :: Parser Text
    word =
      T.pack <$> many letterChar

-- | [ modid . ] varid
qvarid :: Parser Qual
qvarid =
  lexeme $ modids (CVarId <$> varid)

-- | [ modid . ] conid
qconid :: Parser Qual
qconid =
  lexeme $ modids (CConId <$> conid)

-- | [ modid . ] tycon
qtycon :: Parser Qual
qtycon =
  lexeme $ modids (CConId <$> tycon)

-- | [ modid . ] tycls
qtycls :: Parser Qual
qtycls =
  lexeme $ modids (CConId <$> tycls)

-- | [ modid . ] varsym
qvarsym :: Parser Qual
qvarsym =
  lexeme $ modids (CVarSym <$> varsym)

-- | [ modid . ] consym
qconsym :: Parser Qual
qconsym =
  lexeme $ modids (CConSym <$> consym)
