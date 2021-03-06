module Rejig.Lang
where

--------------------------------------------------------------------------------

-- standard imports

import Text.Megaparsec

import qualified Data.Set as Set
import qualified Data.Text as Text

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

runReader' :: r -> Reader r c -> c
runReader' = flip runReader

postValidate ::
  (a -> Either Text a) ->
  Parser a ->
  Parser a
postValidate validate p = do
  o <- getOffset
  r <- p
  either
    (parseError . FancyError o . Set.singleton . ErrorFail . Text.unpack)
    (pure . id)
    $ validate r

applyOr :: [(a -> Bool)] -> a -> Bool
applyOr fs a =
 or $ fs <*> [a]
