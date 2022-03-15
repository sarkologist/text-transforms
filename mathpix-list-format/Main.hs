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
import Lucid

import Turtle as T (stdin, lineToText, inproc, die, empty, fold)

getText = T.fold stdin (F.foldMap (\l -> lineToText l <> "\n") id)

data Content a = Line a | Math [a] deriving Show

lineContent = many' (notChar '\n')

line = lineContent <* endOfLine <|> lineContent

blockMath = do
  string "$$"
  endOfLine
  manyTill line (string "$$")

contents = manyTill (fmap Math blockMath <|> fmap Line line) endOfInput

asHtml :: (Monad m, Term (HtmlT m ()) result) => Content String -> result
asHtml (Math ls) = ul_ $ li_ $ do
  "$$"
  sequence (intersperse (br_ []) . map toHtml $ ls)
  "$$"
asHtml (Line l) = li_ (toHtml l)

formatList :: (Monad m, Term (HtmlT m ()) result) => [Content String] -> result
formatList ls = ul_ (mapM_ asHtml ls)


go = do
  source <- getText
  case parseOnly contents source of
    Left err -> die (T.pack err)
    Right result -> do
      LTIO.putStrLn . renderText . formatList $ result

main :: IO ()
main = go
