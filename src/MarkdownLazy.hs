{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

import MarkdownParse (withinMany)

import Data.Text as T

import Text.Parsec
import Control.Lens (Prism', prism, prism', withPrism)
import Control.Applicative hiding (many)

type Parser a = Parsec Text () a

italic :: Parser Italic
italic = Italic . pack <$> withinMany (char '*') (noneOf (['*']))

data Italic = Italic Text deriving Show
newtype Context = Context Text deriving Show

type Optic a b = Prism' (a, Context) (b, Context)

withPrism' :: Prism' s a -> ((a -> s) -> (s -> Maybe a) -> r) -> r
withPrism' p cont = withPrism p $ \build match ->
    let match' s = case match s of
          Right x -> Just x
          Left _ -> Nothing
    in cont build match'

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

i :: Optic Text Italic
i = prism' build match
  where
    match = parseInContext italic
    build (Italic txt, Context after) = ("*" <> txt <> "*", Context after)

eitherToMaybe e = case e of
        Left _ -> Nothing
        Right x -> Just x
