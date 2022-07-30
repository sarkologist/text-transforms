{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as T

import Control.Monad.State
import Control.Applicative
import qualified Control.Foldl as F
import Data.Maybe
import Data.Monoid
import Data.List

import Data.Attoparsec.Text       as Atto
import Data.Attoparsec.Combinator       as Atto
import Lucid

import Turtle as T (stdin, lineToText, inproc, die, empty, fold)

getText = T.fold stdin (F.foldMap (\l -> lineToText l <> "\n") id)

data Content a = Content [Item a] deriving Show
data Item a = Markdown a | InlineMath a | BlockMath a deriving Show

many1Until p end = (:) <$> p <*> manyTill p (() <$ lookAhead end <|> endOfInput)

within left right p = left *> many1Until p right <* right
between delim = within delim delim

alternating p q = ((:) <$> p <*> alternating q p) <|> pure []

markdown = Markdown <$> many1Until (notChar '$') (char '$')

inlineMath = InlineMath <$> between (char '$') (notChar '$')
blockMath = BlockMath <$> between (string "$$") (notChar '$')

everything = alternating markdown (blockMath <|> inlineMath)

go = do
  source <- getText
  case parseOnly everything source of
    Left err -> die (T.pack err)
    Right result -> return ()

main :: IO ()
main = go

