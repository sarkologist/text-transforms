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
import Data.Char (isSpace)

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

markdown endWith = Markdown <$> many1UntilNonGreedy markdownItems (endWith <|> eof)
  where

    markdownItems = choice . fmap try $ [
        Blockquote <$> blockquote
      , newlineMarkdown
      , MarkdownBullets <$> bullets 0
      , MarkdownTable <$> table
      ] ++ fmap header [1..6]
      ++ [Basic <$> markdownItemsBasic]


blockquote = endOfLine *> string "> " *> many1Until markdownItemsBasic endOfLine

header n = Header n <$>
  between (string (take n (repeat '#')) *> char ' ') endOfLine (many1 markdownItemsBasic)

markdownItemsBasic = choice . fmap try $ [
    highlight, bold, italic, link, tag, BasicInline <$> base (oneOf "*=" <||> string "[[")
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

-- obsidian autocomplete adds a space after tags
tag = Tag <$> (char '#' *> many1Until (oneOf tagChar) (lookAhead (noneOf tagChar)) <* optional (char ' '))
  where tagChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['-']

x <||> y = void x <|> void y

unmarked endWith = Unmarked <$> many1UntilNonGreedy anyChar (endWith <||> char '\n' <|> eof)
inlineMath = InlineMath <$> withinMany (char '$') anyChar

bullets level = Bullets <$> many1 (base <|> check *> recurse)
  where
    base = try (bulletLeaf level)
    recurse = BulletRecurse <$> bullets (level+1)

    check = try (lookAhead (bulletNesting (level+1)))

bulletLeaf level = between (bulletNesting level *> string "- ") (endOfLine <||> eof) $
  BulletLeaf <$> many1 markdownItemsBasic

bulletNesting level = string (mconcat (replicate level "  ")) <|> string (replicate level '\t')

table = try $ do
  headerCells <- tableRow
  separatorCells <- tableSeparatorRow
  bodyRows <- many tableRow
  let columnCount = length headerCells
  if columnCount == separatorCells && all ((== columnCount) . length) bodyRows
    then return (Table headerCells bodyRows)
    else fail "table rows must all have the same number of columns"

tableRow = try $ do
  rawCells <- tableCells <$> tableLine
  case traverse parseTableCell rawCells of
    Left err -> fail (show err)
    Right cells -> return cells

tableSeparatorRow = try $ do
  rawCells <- tableCells <$> tableLine
  if not (null rawCells) && all tableSeparatorCell rawCells
    then return (length rawCells)
    else fail "expected table separator row"

tableLine = try $ do
  line <- manyTill anyChar (try (endOfLine <||> eof))
  if '|' `elem` line
    then return line
    else fail "expected table row"

tableCells line = fmap (T.unpack . T.strip) . T.splitOn "|" . stripOuterPipes . T.strip $ T.pack line
  where
    stripOuterPipes txt = stripTrailingPipe . stripLeadingPipe $ txt
    stripLeadingPipe txt =
      case T.uncons txt of
        Just ('|', rest) -> rest
        _ -> txt
    stripTrailingPipe txt =
      case T.unsnoc txt of
        Just (rest, '|') -> rest
        _ -> txt

tableSeparatorCell cell =
  let stripped = filter (not . isSpace) cell
      withoutAlignment = dropWhile (== ':') . reverse . dropWhile (== ':') . reverse $ stripped
  in length withoutAlignment >= 3 && all (== '-') withoutAlignment

parseTableCell cell = parse (many markdownItemsBasic <* eof) "" (T.pack cell)

blockMath = BlockMath <$> betweenMany (string "$$") (string "$$") anyChar <* (endOfLine <||> eof)

tikzDiagram = TikzDiagram <$> betweenMany tikzStart (try (string "\n\\end{document}\n```")) anyChar <* endOfLine

tikzStart = string "```tikz\n" *> many1UntilNonGreedy anyChar (string "\\begin{tikzcd}")

everything :: ParsecT T.Text u Identity (Content String)
everything = fmap Content . many1 . choice . fmap try $ [ blockMath, tikzDiagram, markdown (string "$$" <||> tikzStart) ]
