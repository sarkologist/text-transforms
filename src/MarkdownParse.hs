{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module MarkdownParse where

import Types

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

within delim p = delim *> p <* delim
withinMany delim p = delim *> many1Until p (lookAhead delim) <* delim

markdown = Markdown <$> many1UntilNonGreedy markdownItems (() <$ string "$$" <|> eof)
  where

    markdownItems = choice . fmap try $ [
        newlineMarkdown
      , MarkdownBullets <$> bullets 0
      , Basic <$> markdownItemsBasic
      ] ++ fmap header [1..6]

header n = Header n <$>
  between (string (take n (repeat '#')) *> char ' ') endOfLine (many1 (markdownItemsBasic))

markdownItemsBasic = choice . fmap try $ [
    bold, italic, BasicInline <$> base
  ]

base = choice [ inlineMath, unmarked ]

newlineMarkdown = Newline . (:[]) <$> endOfLine
bold = Bold <$> withinMany (string "**") base
italic = Italic <$> withinMany (char '*') base

unmarked = Unmarked <$> many1UntilNonGreedy anyChar (() <$ oneOf  "$*#\n" <|> eof)
inlineMath = InlineMath <$> withinMany (char '$') anyChar

bullets level = Bullets <$> many1 (base <|> check *> recurse)
  where
    base = bulletLeaf level
    recurse = BulletRecurse <$> bullets (level+1)

    check = try (lookAhead (bulletNesting (level+1)))

bulletLeaf level = between (bulletNesting level *> string "- ") (() <$ endOfLine <|> eof) $
  BulletLeaf <$> many1 markdownItemsBasic

bulletNesting level = string (mconcat (replicate level "  ")) <|> string (replicate level '\t')

blockMath = BlockMath <$> withinMany (string "$$") anyChar

everything :: ParsecT T.Text u Identity (Content String)
everything = fmap Content . many1 . choice . fmap try $ [ blockMath, markdown ]
