module Main where

import qualified Control.Exception as Ex
import qualified Data.Text as T
import Data.Text as T
import Text.Megaparsec

import Text.PrettyPrint (render)
import Rejig.Parser (importsP)
import Rejig.Pretty (showPretty, Pretty)
import Rejig.Settings
import Rejig.IESorter (sortImports)

main :: IO ()
main = putStrLn "Hello, Haskell!"

mkDefaultSettings :: IO Settings
mkDefaultSettings =
  pure $ Settings
   { _sQualifiedStyle = QualifiedPre
   , _sGroupByPrefix = ["DA.", "Rejig"]
   , _sShowGroupComments = True
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
      case runParser importsP "test" txt of
        Left bundle -> writeFile "result.txt" (errorBundlePretty bundle)
        Right res -> writeFile "result.txt" $
          render $ runReader (showPretty $ runReader (sortImports res) settings) settings

      pure ()

-- parseFile :: FilePath -> IO (Either Ex.SomeException Text)
-- parseFile path = do
  -- result <- Ex.try (readFileText path) :: IO (Either Ex.SomeException Text)
  -- case result of
    -- Left e -> pure $ Left
    -- Right txt ->
      -- Parser.import
