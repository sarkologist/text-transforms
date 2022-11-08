{-# LANGUAGE OverloadedStrings #-}
module Main where

import MarkdownLazy
import LazyParseTransforms
import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Control.Foldl as F
import Data.Fix

import Turtle as T (stdin, lineToText, inproc, die, empty, fold)

getText = T.fold stdin (F.foldMap (\l -> lineToText l <> "\n") id)

unindentHeaders = flip over (\level -> max (level-1) 1)  (text . allTheHeaders . _1 . _Left . level)
unindentBullets = flip over (\(Fix (B lvl content@(Fix (Plain txt)))) -> if lvl > 0 then Fix (B (lvl-1) content) else Fix (Plain (txt <> "\n"))) (text . many' bullet . _1)

main :: IO ()
main = TIO.putStr . unindentBullets . unindentHeaders =<< getText
