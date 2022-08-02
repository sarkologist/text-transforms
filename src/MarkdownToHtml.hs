{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module MarkdownToHtml where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as T

import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Functor.Identity

import Text.Parsec
import Lucid


data Content a = Content [Item a] deriving (Eq, Show)
data Item a = Markdown [MarkdownItem a] | InlineMath a | BlockMath a deriving (Eq, Show)
data MarkdownItem a =
    Basic (MarkdownItemBasic a)
  | Header Int [ MarkdownItemBasic a ]
  deriving (Eq, Show)

data MarkdownItemBasic a =
    Unmarked a
  | Italic a
  | Bold a
  deriving (Eq, Show)

many1Until :: (Stream s m t, Show end) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Until p end = do
  notFollowedBy end
  first <- p
  rest <- manyTill p end
  return (first:rest)

many1UntilNonGreedy :: (Stream s m t, Show end) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1UntilNonGreedy p end = do
  notFollowedBy end
  (:) <$> p <*> (many1UntilNonGreedy p end <|> [] <$ lookAhead end)

within delim p = delim *> many1Until p (lookAhead delim) <* delim

alternating p q = ((:) <$> p <*> alternating q p) <|> pure []

markdown = Markdown <$> many1UntilNonGreedy markdownItems (() <$ oneOf "$" <|> eof)
  where

    markdownItems = choice [
        Basic <$> markdownItemsBasic "$*#"
      , header 1
      ]

header n = Header n <$>
  between (string (take n (repeat '#')) *> char ' ') endOfLine (many1 (markdownItemsBasic "$*#\n"))

markdownItemsBasic ignore = choice . fmap try $ [
    bold, italic, unmarked ignore
  ]

unmarked :: (Monad m) => [Char] -> ParsecT T.Text u m (MarkdownItemBasic [Char])
unmarked ignore = Unmarked <$> many1UntilNonGreedy anyChar (() <$ oneOf ignore <|> eof)
bold = Bold <$> within (string "**") (noneOf "*")
italic = Italic <$> within (char '*') (noneOf "*")

inlineMath = InlineMath <$> within (char '$') anyChar
blockMath = BlockMath <$> within (string "$$") anyChar

everything :: ParsecT T.Text u Identity (Content String)
everything = fmap Content . many1 . choice . fmap try $ [ blockMath, inlineMath, markdown ]
