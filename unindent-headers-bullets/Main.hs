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

main :: IO ()
main = TIO.putStr . unindentBulletIntoSubheader "\t" =<< getText
