module Main where

import qualified Control.Exception as Ex
import Rejig.Sorter
import Rejig.Parser
import Rejig.Pretty (showPretty)
import Rejig.Settings
import Text.Megaparsec
import Text.PrettyPrint (render)
import qualified Options.Applicative as O
import System.FilePath (splitExtension)
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
  O.flag' StdInput $
    O.long "stdin"
    <> O.help "read from stdin"

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
      <> O.help "group by prefixes (separate by whitespace)")

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
  <> O.progDesc "format module header"
  <> O.header "rejig - sort and format module header declarations"

main :: IO ()
main = do
  settings <- O.execParser opts

  case _sInput settings of
    FileInput path -> do
      putStrLn . T.unpack =<< TIO.readFile path
    StdInput ->
      -- putStrLn . T.unpack =<< TIO.getContents
      runFormatter settings =<< TIO.getContents

runFormatter :: Settings -> Text -> IO ()
runFormatter settings content = do
  putStrLn $
    case runParser parseSourceP "test" content of
      Left bundle -> errorBundlePretty bundle
      Right res ->
        render $
          runReader (showPretty $ runReader (sortParsedSource res) settings) settings


mkDefaultSettings :: IO Settings
mkDefaultSettings =
  pure $
    Settings
      { _sInput = StdInput
      , _ssrcLang = Haskell
      , _sGroupByPrefix = ["DA.", "Rejig"]
      , _sDisplayGroupTitle = not True
      , _sImportBorderTop = True
      , _sImportBorderBottom = True
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
