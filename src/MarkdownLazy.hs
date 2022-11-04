{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MarkdownLazy where

import MarkdownParse (withinMany)

import LazyParseTransforms

import Data.Text as T
import Text.Parsec hiding (choice)
import Control.Lens (prism')
import Control.Lens.TH

newtype Italic = Italic { _unItalic :: Text } deriving Show
data Header = Header {
  _level :: Int,
  _content :: Text
} deriving Show

i :: Pprism Text Italic
i = prism' build match
  where
    match = parseInContext $ Italic . pack <$> withinMany (char '*') (noneOf (['*']))
    build (Italic txt, ctx) = ("*" <> txt <> "*", ctx)

h :: Int ->  Pprism Text Header
h n = prism' build match
  where
    match = parseInContext $ Header n . pack <$> between (string (hashes n) *> char ' ') endOfLine (many1 (noneOf ['\n']))
    build (Header k txt, ctx) = (pack (hashes k) <> " " <> txt <> "\n" , ctx)

    hashes k = Prelude.take k (Prelude.repeat '#')

headers :: Ptraversal Text Header
headers = choice' [
    ChoiceTraversal (h 1)
  , ChoiceTraversal (h 2)
  , ChoiceTraversal (h 3)
  , ChoiceTraversal (h 4)
  , ChoiceTraversal (h 5)
  , ChoiceTraversal (h 6)
  ]

skip :: [Char] -> Pprism Text Text
skip toSkip = prism' id match
  where
    match = parseInContext $ pack <$> many1 (noneOf toSkip)

allTheHeaders :: Ptraversal Text (Either Header Text)
allTheHeaders = many' (headers <||> skip "#")

makeLenses ''Italic
makeLenses ''Header
makePrisms ''Header
makePrisms ''Italic
