module Main where

import qualified Control.Exception as Ex
import Rejig.Sorter
import Rejig.Parser
import Rejig.Pretty (showPretty)
import Rejig.Settings
import Text.Megaparsec
import Text.PrettyPrint (render)
import qualified Options.Applicative as O
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


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
      , _ssrcLang = Haskell
      , _sPrefixGroups = ["DA.", "DA.Finance", "Test", "Rejig", "Danban"]
      , _sDisplayGroupTitle = not True
      , _sImportBorderTop = not True
      , _sImportBorderBottom = not True
      }

-- runPrettyRender :: Pretty a => Settings -> a -> String
-- runPrettyRender settings source =
-- render $ runReader (showPretty sorted) settings

-- where
-- sorted = runReader (sortImports source) settings

-- writeFormattedFile :: ReaderT Settings IO ()
-- writeFormattedFile = do
-- settings <- ask

parseFile :: FilePath -> IO ()
parseFile path = do
  result <- Ex.try (readFileText path) :: IO (Either Ex.SomeException Text)
  settings <- mkDefaultSettings

  case result of
    Left e -> pure ()
    Right txt -> do
      -- case runParser singleLineCommentsP "test" txt of
      -- case runParser leadingCommentsP "test" txt of
      case runParser parseSourceP "test" txt of
        Left bundle -> writeFile "result.txt" (errorBundlePretty bundle)
        Right res ->
          writeFile "result.txt" $
            -- render $ runReader (showPretty $ runReader (sortImports res) settings) settings
            render $
              runReader (showPretty $ runReader (sortParsedSource res) settings) settings
            -- render $ show res

      pure ()

-- parseFile :: FilePath -> IO (Either Ex.SomeException Text)
-- parseFile path = do
-- result <- Ex.try (readFileText path) :: IO (Either Ex.SomeException Text)
-- case result of
-- Left e -> pure $ Left
-- Right txt ->
-- Parser.import
