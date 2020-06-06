module Text.Parsing.Arguments.Value
  ( spaced
  ) where

import Prelude

import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.String as S
import Text.Parsing.Parser.Combinators as C

import Text.Parsing.String.Repetition as R
import Text.Parsing.Combinators.Validation as V

unquoted :: forall a m b. Monad m => ParserT a m b -> ParserT a m Char -> ParserT a m String
unquoted delimiter = \p -> do
  x <- R.until (V.fail delimiter *> p) delimiter
  pure x

quoted :: forall a m b c. Monad m => ParserT a m b -> ParserT a m c -> ParserT a m Char -> ParserT a m String
quoted quote delimiter = \p -> do
  _ <- quote
  x <- R.until (V.fail quote *> p) (quote *> delimiter)
  _ <- quote
  pure x

separated :: forall a m b. Monad m => S.StringLike a => ParserT a m b -> ParserT a m String
separated delimiter = do
  x <- C.lookAhead $ S.anyChar
  case x of
    '\'' -> singleQuoted delimiter $ S.anyChar
    '"'  -> doubleQuoted delimiter $ S.anyChar
    _    -> unquoted delimiter     $ S.anyChar
  where
    singleQuoted = quoted $ S.char '\''
    doubleQuoted = quoted $ S.char '"'

delimited :: forall a m. Monad m => S.StringLike a => ParserT a m Char -> ParserT a m String
delimited delimiter = separated $ C.choice [delimiter *> pure unit, S.eof]

-- | Consumes an optionally single-quoted or double-quoted argument value, where argument names and values are separated by a space.
-- | Consumes the single or double quote characters wrapping the argument value, but does not consume the trailing space or eof delimiter.
-- | Argument values can consist of any character, except a space if the value is unquoted.
-- | e.g. "'!@abcdefghijklmnopqrstuvwxyz#$%^&*()_+" '" is valid input, but "!@abcdefghijklmnopqrstuvwxyz#$%^&*()_+" " is not. 
spaced :: forall a m. Monad m => S.StringLike a => ParserT a m String
spaced = delimited $ S.char ' '
