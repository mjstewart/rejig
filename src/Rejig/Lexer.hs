module Rejig.Lexer where

import Control.Monad.Combinators
import Rejig.Lang ( postValidate, Parser )
import Rejig.Ast
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
    [ '('
    , ')'
    , ','
    , ';'
    , '['
    , ']'
    , '`'
    , '{'
    , '}'
    ]

reservedId :: Set Text
reservedId =
  Set.fromList
    [ "case"
    , "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "module"
    , "newtype"
    , "of"
    , "then"
    , "type"
    , "where"
    , "_"
    , "template"
    ]

reservedOp :: Set Text
reservedOp =
  Set.fromList
    [ ".."
    , ":"
    , "::"
    , "="
    , "\\"
    , "|"
    , "<-"
    , "->"
    , "@"
    , "~"
    , "=>"
    ]

ascSymbols :: Set Char
ascSymbols =
  Set.fromList
  [ '!'
  , '#'
  , '$'
  , '%'
  , '&'
  , '*'
  , '+'
  , '.'
  , '/'
  , '<'
  , '='
  , '>'
  , '?'
  , '@'
  , '\\'
  , '^'
  , '|'
  , '-'
  , '~'
  ]

-- | newline space consumer that doesnt skip comments
scn :: Parser ()
scn =
  L.space space1 empty empty

-- | literal space consumer that doesnt skip comments
sce :: Parser ()
sce =
  L.space (void $ takeWhile1P Nothing (== ' ')) empty empty

-- | symbol with newline space consumer that doesnt skip comments
symbol :: Text -> Parser Text
symbol = L.symbol scn

-- | lexeme with newline space consumer that doesnt skip comments
lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

comma :: Parser ()
comma =
  void $ symbol ","

dot :: Parser ()
dot =
  void $ symbol "."

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pragma :: Parser a -> Parser a
pragma = between (symbol "{-#") (symbol "#-}")

takeTillNewLine :: Parser Text
takeTillNewLine =
  takeWhileP Nothing (/= '\n')

-- ends with a new line and consumes all space up until the next potential comment
singleLineCommentP :: Parser Text
singleLineCommentP =
  (symbol "--" *> takeTillNewLine) <* newline <* sce

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

tick :: Parser Char
tick =
  char '\''

small :: Parser Char
small =
  lowerChar <|> char '_'

large :: Parser Char
large =
  upperChar

wrapParens :: Text -> Text
wrapParens x = "(" <> x <> ")"

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
    parser = try ascSymbol <|> try symbolChar <|> punctuationChar
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
    parser = lexeme $ T.cons <$> small <*> body

    body :: Parser Text
    body = many
      (choice [
        small
      , large
      , digitChar
      , tick
      ]) <&> T.pack

    check result =
      if Set.member result reservedId
      then Left $ show result <> " is a reserved identifier"
      else Right result

-- | large {small | large | digit | ' }
conid :: Parser Text
conid =
  lexeme $ T.cons <$> large <*> body
  where
    body :: Parser Text
    body = many
      (choice [
        small
      , large
      , digitChar
      , tick
      ]) <&> T.pack

-- | ( symbol {symbol | :})<reservedop | dashes>
-- dashes implicitly ignored since lexeme/symbol space consumer ignores them
varsym :: Parser Text
varsym =
  postValidate check parser
  where
    parser = lexeme $ wrapParens <$> (parens $ lexeme (T.cons <$> sym <*> body))
    body = T.pack <$> many (try sym <|> char ':')

    check result =
      if Set.member result reservedOp
      then Left $ show result <> " is a reserved operator"
      else Right result

-- | (: {symbol | :})<reservedop>
consym :: Parser Text
consym =
  postValidate check parser
  where
    parser = lexeme $ wrapParens <$> (parens $ lexeme (T.cons <$> char ':' <*> body))
    body = T.pack <$> many (try sym <|> char ':')

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
  Eg. The section [A, B, C] use the same parser since these are the qualified conids,
  but ??? can be something else like a varid.

  A.B.C.???

  The lookAhead is a preprocessing step, similar to doing a string split to identify the last
  element to run a different parser over.
 -}
modids :: Parser Text -> Parser Qual
modids endP = do
  qualifiedDots <- lookAhead $ length <$> isModId `sepBy` dot
  Qual
    <$> count (qualifiedDots - 1) (modid <* dot)
    <*> endP

{-
  qvarid -> [ modid . ] varid
  qconid -> [ modid . ] conid
  qtycon -> [ modid . ] tycon
  qtycls -> [ modid . ] tycls
  qvarsym -> [ modid . ] varsym
  qconsym -> [ modid . ] consym
-}
isModId :: Parser ()
isModId =
  void $ choice [
    try conid
  , try varid
  , try consym
  , try varsym
  ]

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
