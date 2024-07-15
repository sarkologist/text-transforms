{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module MarkdownParse where

import Types

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as T

import Control.Monad.State
import Control.Monad (void, MonadPlus (mzero))
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
withinMany delim = betweenMany delim delim
betweenMany left right p = left *> many1Until p (lookAhead right) <* right

markdown = Markdown <$> many1UntilNonGreedy markdownItems (() <$ string "$$" <|> eof)
  where

    markdownItems = choice . fmap try $ [
        newlineMarkdown
      , MarkdownBullets <$> bullets 0
      ] ++ fmap header [1..6]
      ++ [Basic <$> markdownItemsBasic]

header n = Header n <$>
  between (string (take n (repeat '#')) *> char ' ') endOfLine (many1 markdownItemsBasic)

markdownItemsBasic = choice . fmap try $ [
    highlight, bold, italic, link , BasicInline <$> base (oneOf "*=" <||> string "[[")
  ]

base endWith = choice [ inlineMath, unmarked (char '$' <||> endWith) ]

newlineMarkdown = Newline . (:[]) <$> endOfLine
bold = Bold <$> withinMany (string "**") (base (char '*'))
italic = Italic <$> withinMany (char '*') (base (char '*'))
highlight = Highlight <$> withinMany (string "==") (base (char '='))
link = Link <$> between (string "[[") (string "]]") (try onePart <|> twoPart)
  where
    onePart = many1Until (base (string "]]" <||> char '|')) (lookAhead (string "]]"))
    twoPart = many1Until (base (char '|')) (char '|') *> onePart

x <||> y = void x <|> void y

unmarked endWith = Unmarked <$> many1UntilNonGreedy anyChar (endWith <||> char '\n' <|> eof)
inlineMath = InlineMath <$> withinMany (char '$') anyChar

bullets level = Bullets <$> many1 (base <|> check *> recurse)
  where
    base = try (bulletLeaf level)
    recurse = BulletRecurse <$> bullets (level+1)

    check = try (lookAhead (bulletNesting (level+1)))

bulletLeaf level = between (bulletNesting level *> string "- ") (() <$ endOfLine <|> eof) $
  BulletLeaf <$> many1 markdownItemsBasic

bulletNesting level = string (mconcat (replicate level "  ")) <|> string (replicate level '\t')

blockMath = BlockMath <$> withinMany (string "$$") anyChar

everything :: ParsecT T.Text u Identity (Content String)
everything = fmap Content . many1 . choice . fmap try $ [ blockMath, markdown ]
