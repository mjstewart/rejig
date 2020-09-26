module Main where

import qualified Control.Exception as Ex
import qualified Data.Text as T
import qualified Rejig.Parser as Parser

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- parseFile :: FilePath -> IO (Either Ex.SomeException Text)
-- parseFile path = do
  -- result <- Ex.try (readFileText path) :: IO (Either Ex.SomeException Text)
  -- case result of
    -- Left e -> pure $ Left
    -- Right txt ->
      -- Parser.import
