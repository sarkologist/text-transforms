{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import MarkdownParse (withinMany)

import Data.Text as T

import Text.Parsec hiding (many, choice)
import Control.Lens (Iso', Choice, Optic', alongside, Prism', prism, prism', withPrism, iso, swapped, mapping)
import Control.Lens.TH
import Control.Applicative hiding (many, some)

type Parser a = Parsec Text () a
newtype Context = Context Text deriving Show
type P p f a b = Optic' p f (a, Context) (b, Context)
type Pprism a b = forall p f. (Choice p, Applicative f) => P p f a b

withPrism' :: Prism' s a -> ((a -> s) -> (s -> Maybe a) -> r) -> r
withPrism' p cont = withPrism p $ \build match ->
    let match' s = case match s of
          Right x -> Just x
          Left _ -> Nothing
    in cont build match'
inContext p = alongside p id

_nonEmpty :: Iso' (a, Either (a,[a]) ()) (a,[a])
_nonEmpty = iso toList fromList
  where
    toList (a, Left (x,y)) = (a, x:y)
    toList (a, Right ()) = (a, [])

    fromList (a, []) = (a, Right ())
    fromList (a, x:y) = (a, Left (x,y))

_list :: Iso' (Either (a,[a]) ()) [a]
_list = iso toList fromList
  where
    toList (Left (x,xs)) = x:xs
    toList (Right ()) = []

    fromList (x:xs) = Left (x,xs)
    fromList [] = Right ()

none :: Pprism Text ()
none = prism' (\((), ctx) -> ("", ctx)) match
  where
    match (txt, Context ctx) = Just ((), Context (txt <> ctx))

many :: Pprism Text x -> Pprism Text [x]
many p = many' p . swapped . mapping _list . swapped

some :: Pprism Text a -> Pprism Text (a,[a])
some p = andThen p (many' p) . swapped . mapping _nonEmpty . swapped

many' :: Pprism Text a -> Pprism Text (Either (a,[a]) ())
many' p = (<||>) (some p) none

newtype ChoicePrism a b = ChoicePrism { unChoicePrism :: Pprism a b }

choice :: [ ChoicePrism a b ] -> Pprism a (b, Int)
choice ps = unChoicePrism $ go 0 ps
  where
    go n (p:[]) = ChoicePrism $ unChoicePrism p . swapped . mapping (index n) . swapped
    go n (p:rest) = ChoicePrism $ (unChoicePrism p <||> unChoicePrism (go (n+1) rest)) . swapped . mapping (marshal n) . swapped
    go _ [] = error "empty choice"

    index :: Int -> Iso' a (a,Int)
    index n = iso (\a -> (a,n)) (\(a,n) -> a)

    marshal :: Int -> Iso' (Either b (b, Int)) (b, Int)
    marshal n = iso merge split
      where
        merge (Left b) = (b, n)
        merge (Right (b, k)) = (b, k)

        split (b, k) | k > 0  = Right (b, k)
        split (b, k) | k == 0 = Left b

headers :: Pprism Text (Header, Int)
headers = choice [
   ChoicePrism (h 1),
   ChoicePrism (h 2),
   ChoicePrism (h 3),
   ChoicePrism (h 4),
   ChoicePrism (h 5),
   ChoicePrism (h 6)
 ]

-- cannot use Control.Lens.Traversal.failing because it composes Prisms into Traversals
-- and we need it to stay a Prism because `andThen` takes Prisms
-- it returns Either because we need to build according to the correct Prism
(<||>) :: Pprism a x -> Pprism a y -> Pprism a (Either x y)
(<||>) first second = withPrism' first $ \b m ->
  withPrism' second $ \b' m' ->
  let
    match (a, ctx) =  case m (a, ctx) of
      Just (x,ctx) -> Just (Left x, ctx)
      Nothing -> case m' (a, ctx) of
        Just (y, ctx) -> Just (Right y, ctx)
        Nothing -> Nothing

    build (Left x, ctx) = b (x, ctx)
    build (Right y, ctx) = b' (y, ctx)
   in prism' build match

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
