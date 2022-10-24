{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

import MarkdownParse (withinMany)

import qualified Data.Text as T

import Text.Parsec
import Control.Lens (Prism', prism, prism', withPrism)
import Control.Applicative hiding (many)

italic = Italic . T.pack <$> withinMany (char '*') (noneOf (['*']))

data Italic = Italic T.Text deriving Show
newtype Context = Context T.Text deriving Show


data Optic a b = Optic {
    matchOptic :: a -> Context -> Maybe (b, Context)
  , buildOptic :: Context -> b -> (a, Context)
}

andThen :: Optic a x -> Optic T.Text y -> Optic a (x, y)
andThen (Optic m b) (Optic m' b') = Optic match build
  where
    match a ctx = case m a ctx of
      Just (x, (Context unconsumed)) -> case m' unconsumed (Context "") of
        Just (y, unconsumed') -> Just ((x,y), unconsumed')
        Nothing -> Nothing
      Nothing -> Nothing

    build ctx (x,y) = case b' ctx y of
      (t, Context ctx') -> b (Context (t <> ctx')) x

parseInContext :: Parsec T.Text () a -> T.Text -> Context -> Maybe (a, Context)
parseInContext p input (Context after) = eitherToMaybe $
  parse (liftA2 (\parsed unconsumed ->
    (parsed, Context (unconsumed <> after)))
    p getInput) "" input

i :: Optic T.Text Italic
i = Optic {
    matchOptic = parseInContext italic
  , buildOptic = \(Context after) (Italic txt) -> ("*" <> txt <> "*", Context after)
}

asPrism :: Optic a b -> Prism' (a, Context) (b, Context)
asPrism (Optic match build) = prism' build' match'
  where
    build' (b, ctx) = build ctx b
    match' (a, ctx) = match a ctx

eitherToMaybe e = case e of
        Left _ -> Nothing
        Right x -> Just x
