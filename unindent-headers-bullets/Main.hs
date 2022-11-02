{-# LANGUAGE OverloadedStrings #-}
module Main where

import MarkdownLazy
import LazyParseTransforms
import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Control.Foldl as F

import Turtle as T (stdin, lineToText, inproc, die, empty, fold)

getText = T.fold stdin (F.foldMap (\l -> lineToText l <> "\n") id)

unindentHeaders = flip over (\level -> max (level-1) 1)  (text . allTheHeaders . _1 . _Left . level)

main :: IO ()
main = TIO.putStr . unindentHeaders =<< getText
