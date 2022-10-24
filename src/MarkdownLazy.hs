{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import MarkdownParse (withinMany)

import Data.Text as T

import Text.Parsec
import Control.Lens (alongside, Prism', prism, prism', withPrism)
import Control.Lens.TH
import Control.Applicative hiding (many)

type Parser a = Parsec Text () a
newtype Context = Context Text deriving Show
type Optic a b = Prism' (a, Context) (b, Context)

withPrism' :: Prism' s a -> ((a -> s) -> (s -> Maybe a) -> r) -> r
withPrism' p cont = withPrism p $ \build match ->
    let match' s = case match s of
          Right x -> Just x
          Left _ -> Nothing
    in cont build match'

inContext p = alongside p id

andThen :: Optic a x -> Optic Text y -> Optic a (x, y)
andThen first second = withPrism' first $ \b m ->
  withPrism' second $ \b' m' ->
  let
    match (a, ctx) = case m (a, ctx) of
      Just (x, (Context unconsumed)) -> case m' (unconsumed, (Context "")) of
        Just (y, unconsumed') -> Just ((x,y), unconsumed')
        Nothing -> Nothing
      Nothing -> Nothing

    build ((x,y), ctx) = case b' (y, ctx) of
       (t, Context ctx') -> b (x, (Context (t <> ctx')))
   in prism' build match

parseInContext :: Parser a -> (Text, Context) -> Maybe (a, Context)
parseInContext p (input, (Context after)) = eitherToMaybe $
  parse (liftA2 (\parsed unconsumed ->
    (parsed, Context (unconsumed <> after)))
    p getInput) "" input

newtype Italic = Italic { _unItalic :: Text } deriving Show
data Header = Header {
  _level :: Int,
  _content :: Text
} deriving Show

i :: Optic Text Italic
i = prism' build match
  where
    match = parseInContext $ Italic . pack <$> withinMany (char '*') (noneOf (['*']))
    build (Italic txt, Context after) = ("*" <> txt <> "*", Context after)

h :: Int -> Optic Text Header
h n = prism' build match
  where
    match = parseInContext $ Header n . pack <$> between (string (hashes n) *> char ' ') endOfLine (many1 (noneOf ['\n']))
    build (Header k txt, Context after) = (pack (hashes k) <> " " <> txt <> "\n" , Context after)

    hashes k = Prelude.take k (repeat '#')

eitherToMaybe e = case e of
        Left _ -> Nothing
        Right x -> Just x

makeLenses ''Italic
makeLenses ''Header
