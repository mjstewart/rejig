module Rejig.Lexer where

import Control.Monad.Combinators
import Rejig.Lang ( applyOr, postValidate, Parser )
import Rejig.Ast
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (many, some)

{-| This file contains all the lexing primitives to parse import/export declarations by using
  https://www.haskell.org/onlinereport/syntax-iso.html
-}

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

-- | newline space consumer that skips comments
scn :: Parser ()
scn =
  L.space space1 lineComment blockComment

-- | newline space consumer that doesnt skip comments
scnComment :: Parser ()
scnComment =
  L.space space1 empty empty

-- | literal space consumer that skips comments
sc :: Parser ()
sc =
  L.space (void $ takeWhile1P Nothing (== ' ')) lineComment blockComment

-- | literal space consumer that doesnt skip comments
sce :: Parser ()
sce =
  L.space (void $ takeWhile1P Nothing (== ' ')) empty empty

-- | symbol with newline space consumer that doesnt skip comments
symbolComment :: Text -> Parser Text
symbolComment = L.symbol scnComment

-- | lexeme with newline space consumer that doesnt skip comments
lexemeComment :: Parser a -> Parser a
lexemeComment = L.lexeme scnComment

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

pragma :: Parser a -> Parser a
pragma = between (symbolComment "{-#") (symbolComment "#-}")

takeTillNewLine :: Parser Text
takeTillNewLine =
  takeWhileP Nothing (/= '\n')

-- ends with a new line and consumes all space up until the next potential comment
singleLineCommentP :: Parser Text
singleLineCommentP =
  (symbolComment "--" *> takeTillNewLine) <* newline <* sce

-- collects the inner contents of a {- block comment -}
blockCommentP :: Parser Comment
blockCommentP =
  BlockComment . T.pack <$> ((block "{-" *> manyTill anySingle (string "-}")) <* sce)
  where
    block x = symbol x <* notFollowedBy "#"

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

wrap :: Text -> Text
wrap x = "(" <> x <> ")"

ascSymbol :: Parser Char
ascSymbol =
  postValidate check anySingle
  where
    check result =
      if Set.member result ascSymbols
      then Right result
      else Left $ show result <> " is not a legal ascii symbol"

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
varid :: Parser Text
varid =
  postValidate check parser
  where
    parser = lexeme $ T.cons <$> small <*> takeWhileP Nothing body

    body = applyOr [Char.isLower, Char.isUpper, Char.isDigit, (== '\'')]

    check result =
      if Set.member result reservedId
      then Left $ show result <> " is a reserved identifier"
      else Right result

-- | large {small | large | digit | ' }
conid :: Parser Text
conid =
  lexeme $ T.cons <$> large <*> takeWhileP Nothing body
  where
    body = applyOr [Char.isLower, Char.isUpper, Char.isDigit, (== '\'')]

-- | ( symbol {symbol | :})<reservedop | dashes>
-- dashes implicitly ignored since lexeme/symbol space consumer ignores them
varsym :: Parser Text
varsym =
  postValidate check parser
  where
    parser = lexeme $ wrap <$> (parens $ lexeme (T.cons <$> sym <*> body))
    body = T.pack <$> many (try sym) <|> show <$> char ':'

    check result =
      if Set.member result reservedOp
      then Left $ show result <> " is a reserved operator"
      else Right result

-- | (: {symbol | :})<reservedop>
consym :: Parser Text
consym =
  postValidate check parser
  where
    parser = lexeme $ wrap <$> (parens $ lexeme (T.cons <$> char ':' <*> body))
    body = T.pack <$> (many $ try sym <|> char ':')

    check result =
      if Set.member result reservedOp
      then Left $ show result <> " is a reserved operator"
      else Right result

tyvar :: Parser Text
tyvar = varid

tycon :: Parser Text
tycon = conid

tycls :: Parser Text
tycls = conid

modid :: Parser ConId
modid = ConId <$> conid

{-| This parser is abit involved due to the last element being a different lexeme type.
  Eg. A B C use the same parser, but ??? can be something else.

  A.B.C.???

  The lookAhead is a preprocessing step, similar to doing a string split to identify the last
  element to run a different parser over.
 -}
modids :: Parser Text -> Parser Qual
modids endP = do
  segmentCount <- (dec . length) <$> (lookAhead $ word `sepBy` dot)
  Qual <$> (count segmentCount $ modid <* dot) <*> endP
  where
    dec x = x - 1

    word :: Parser Text
    word =
      T.pack <$> many letterChar

-- | [ modid . ] varid
qvarid :: Parser QVarId
qvarid =
  QVarId <$> (lexeme $ modids varid)

-- | [ modid . ] conid
qconid :: Parser QConId
qconid =
  QConId <$> (lexeme $ modids conid)

-- | [ modid . ] tycon
qtycon :: Parser QConId
qtycon =
  QConId <$> (lexeme $ modids tycon)

-- | [ modid . ] tycls
qtycls :: Parser QConId
qtycls =
  QConId <$> (lexeme $ modids tycls)

-- | [ modid . ] varsym
qvarsym :: Parser QVarSym
qvarsym =
  QVarSym <$> (lexeme $ modids varsym)

-- | [ modid . ] consym
qconsym :: Parser QConSym
qconsym =
  QConSym <$> (lexeme $ modids consym)
