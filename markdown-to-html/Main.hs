{-# LANGUAGE OverloadedStrings #-}
module Main where

import MarkdownParse (everything)
import MarkdownToHtml (markdownToHtml)
import Anki

import Text.Parsec
import Lucid

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import qualified Control.Foldl as F

import Turtle as T (stdin, lineToText, inproc, die, empty, fold)

getText = T.fold stdin (F.foldMap (\l -> lineToText l <> "\n") id)

main :: IO ()
main = do
  source <- getText
  case parse everything "" source of
    Left err -> die (T.pack (show err))
    Right parsed -> do
      LTIO.putStrLn . renderText . markdownToHtml . normaliseHeaders $ parsed

