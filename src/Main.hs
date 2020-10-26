module Main
where

--------------------------------------------------------------------------------

-- standard imports

import Text.Megaparsec

import Text.PrettyPrint
  ( render
  )

import qualified Data.Text.IO as TIO
import qualified Options.Applicative as O

-- imports by Rejig*

import Rejig.Parser
import Rejig.Settings
import Rejig.Sorter

import Rejig.Pretty
  ( showPretty
  )

--------------------------------------------------------------------------------

fileInput :: O.Parser Input
fileInput =
  FileInput <$>
    (O.strOption $
      O.long "file"
      <> O.metavar "FILEPATH"
      <> O.help "input file path")

stdInput :: O.Parser Input
stdInput =
 StdInput <$>
   (O.strOption $
    O.long "stdin"
    <> O.metavar "DESCRIPTION"
    <> O.help "read using stdin including an input source description")

inputP :: O.Parser Input
inputP = fileInput <|> stdInput

srcLangP :: O.Parser SourceLang
srcLangP =
  haskellLang <|> damlLang
  where
    haskellLang =
      O.flag' Haskell $
        O.long "haskell"
        <> O.help "format haskell source"

    damlLang =
      O.flag' Daml $
        O.long "daml"
        <> O.help "format daml source"

prefixP :: O.Parser [Text]
prefixP =
  words <$>
    (O.strOption $
      O.long "prefixes"
      <> O.value ""
      <> O.metavar "ARG1 ARG2 ..."
      <> O.help "import names beginning with matching prefixes are grouped together. Provide from least to most specific order")

importTitleP :: O.Parser Bool
importTitleP =
  O.switch $
    O.long "titles"
    <> O.help "Display import group titles"

importBorderTopP :: O.Parser Bool
importBorderTopP =
  O.switch $
    O.long "border-top"
    <> O.help "Display border at the start of import declarations"

importBorderBottomP :: O.Parser Bool
importBorderBottomP =
  O.switch $
    O.long "border-bottom"
    <> O.help "Display border at the end of import declarations"

settingsP :: O.Parser Settings
settingsP = Settings
  <$> inputP
  <*> srcLangP
  <*> prefixP
  <*> importTitleP
  <*> importBorderTopP
  <*> importBorderBottomP

opts :: O.ParserInfo Settings
opts = O.info (settingsP <**> O.helper)
  $ O.fullDesc
  <> O.progDesc "Sort and format module header declarations"
  <> O.header "Rejig"

main :: IO ()
main = do
  settings <- O.execParser opts

  case _sInput settings of
    FileInput path -> do
      runFormatter path settings =<< TIO.readFile path
    StdInput description ->
      runFormatter description settings =<< TIO.getContents

runFormatter :: String -> Settings -> Text -> IO ()
runFormatter description settings content = do
  putStrLn $
    case runParser parseSourceP description content of
      Left bundle -> toResponse "error" $ errorBundlePretty bundle
      Right res ->
        toResponse "ok" $ render $
          runReader (showPretty $ runReader (sortParsedSource res) settings) settings

toResponse :: String -> String -> String
toResponse status body =
  "{ \"status\": " <> show status <> ", \"data\": " <> show body <> "}"

mkDefaultSettings :: IO Settings
mkDefaultSettings =
  pure $
    Settings
      { _sInput = StdInput "demo"
      , _ssrcLang = Daml
      , _sPrefixGroups = ["Daml", "DA", "DA.Next", "DA.Finance", "Test.MyApp", "Main.MyApp"]
      , _sDisplayGroupTitle = True
      , _sImportBorderTop = True
      , _sImportBorderBottom = True
      }

-- For testing
parseFile :: FilePath -> IO ()
parseFile path = do
  txt <- TIO.readFile path
  settings <- mkDefaultSettings

  case runParser parseSourceP "test" txt of
    Left bundle -> writeFile "test/snippets/result.txt" (errorBundlePretty bundle)
    Right res ->
      writeFile "test/snippets/result.txt" $
        render $
          runReader (showPretty $ runReader (sortParsedSource res) settings) settings
