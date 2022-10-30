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
    build (Italic txt, Context after) = ("*" <> txt <> "*", Context after)

noti :: Pprism Text Text
noti = prism' build match
  where
    match = parseInContext $ pack <$> many (noneOf (['*']))
    build (txt, Context after) = (txt, Context after)

h :: Int ->  Pprism Text Header
h n = prism' build match
  where
    match = parseInContext $ Header n . pack <$> between (string (hashes n) *> char ' ') endOfLine (many1 (noneOf ['\n']))
    build (Header k txt, Context after) = (pack (hashes k) <> " " <> txt <> "\n" , Context after)

    hashes k = Prelude.take k (repeat '#')

headers :: Ptraversal Text Header
headers = choice' [
    ChoiceTraversal (h 1)
  , ChoiceTraversal (h 2)
  , ChoiceTraversal (h 3)
  , ChoiceTraversal (h 4)
  , ChoiceTraversal (h 5)
  , ChoiceTraversal (h 6)
  ]

headersPrism :: Pprism Text (Header, Int)
headersPrism = choice [
    ChoicePrism (h 1)
  , ChoicePrism (h 2)
  , ChoicePrism (h 3)
  , ChoicePrism (h 4)
  , ChoicePrism (h 5)
  , ChoicePrism (h 6)
  ]


notheader :: Pprism Text Text
notheader = prism' build match
  where
    match = parseInContext $ pack <$> many (noneOf (['#']))
    build (txt, Context after) = (txt, Context after)

eitherToMaybe e = case e of
        Left _ -> Nothing
        Right x -> Just x

makeLenses ''Italic
makeLenses ''Header
makePrisms ''Header
makePrisms ''Italic
