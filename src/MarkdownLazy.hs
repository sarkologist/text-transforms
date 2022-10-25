{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import MarkdownParse (withinMany)

import Data.Text as T

import Text.Parsec hiding (many)
import Control.Lens (Iso', Choice, Optic', alongside, Traversal', Prism', prism, prism', withPrism, failing, iso, swapped, mapping)
import Control.Lens.TH
import Control.Applicative hiding (many, some)

type Parser a = Parsec Text () a
newtype Context = Context Text deriving Show
type P p f a b = Optic' p f (a, Context) (b, Context)
type Pprism a b = forall p f. (Choice p, Applicative f) => P p f a b
type Ptraversal a b = forall f. Applicative f => P (->) f a b

withPrism' :: Prism' s a -> ((a -> s) -> (s -> Maybe a) -> r) -> r
withPrism' p cont = withPrism p $ \build match ->
    let match' s = case match s of
          Right x -> Just x
          Left _ -> Nothing
    in cont build match'
inContext p = alongside p id

_cons :: Iso' (x,[x]) [x]
_cons = iso (\(x,y) -> x:y) (\(x:y) -> (x,y))

none :: Pprism Text [a]
none = prism' (\([], ctx) -> ("", ctx)) (\(_, ctx) -> Just ([], ctx))

many :: Pprism Text x -> Pprism Text [x]
many p = many' p

some :: Pprism Text a -> Pprism Text [a]
some p = andThen p (many' p) . swapped . mapping _cons . swapped

many' :: Pprism Text a -> Pprism Text [a]
many' p = failing (some p) none

andThen :: Pprism a x -> Pprism Text y -> Pprism a (x, y)
andThen first second = withPrism' first $ \b m ->
  let
    match (a, ctx) = case m (a, ctx) of
      Just (x, (Context unconsumed)) -> withPrism' second $ \b' m' ->
        case m' (unconsumed, (Context "")) of
          Just (y, unconsumed') -> Just ((x,y), unconsumed')
          Nothing -> Nothing
      Nothing -> Nothing

    build ((x,y), ctx) = withPrism' second $ \b' m' ->
      case b' (y, ctx) of
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

i :: Pprism Text Italic
i = prism' build match
  where
    match = parseInContext $ Italic . pack <$> withinMany (char '*') (noneOf (['*']))
    build (Italic txt, Context after) = ("*" <> txt <> "*", Context after)

h :: Int ->  Pprism Text Header
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
