{-# LANGUAGE OverloadedStrings #-}
module Main where

import MarkdownToHtml (everything)
import Text.Parsec

import qualified Data.Text as T
import qualified Control.Foldl as F

import Turtle as T (stdin, lineToText, inproc, die, empty, fold)

getText = T.fold stdin (F.foldMap (\l -> lineToText l <> "\n") id)

main :: IO ()
main = do
  source <- getText
  case parse everything "" source of
    Left err -> die (T.pack (show err))
    Right result -> return ()

