{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LazyParseTransforms where

import Data.Text as T

import Text.Parsec hiding (choice)
import Control.Lens (Lens', Traversal', Iso', Choice, Optic', alongside, Prism', prism, prism', withPrism, iso, swapped, mapping, _Left, failing, ignored, _1)
import Control.Applicative

type Parser a = Parsec Text () a
newtype Context = Context Text deriving Show
type P p f a b = Optic' p f (a, Context) (b, Context)
type Pprism a b = forall p f. (Choice p, Applicative f) => P p f a b
type Ptraversal a b = forall f. (Applicative f) => P (->) f a b

withPrism' :: Prism' s a -> ((a -> s) -> (s -> Maybe a) -> r) -> r
withPrism' p cont = withPrism p $ \build match ->
    let match' s = case match s of
          Right x -> Just x
          Left _ -> Nothing
    in cont build match'

focusing :: Lens' s Text -> Lens' (s, Context) (Text, Context)
focusing focus = alongside (focus . emptyContext) id . _1

manyOf :: Pprism Text a -> Pprism Text Text -> Ptraversal Text a
manyOf single negative =
  many' (andThen single negative) . mergeContext
    where
      mergeContext :: Iso' ((a,Text), Context) (a, Context)
      mergeContext = iso (\((x,negative), Context ctx) -> (x, Context (negative <> ctx))) (\(x, ctx) -> ((x,""), ctx))

emptyContext :: Iso' Text (Text, Context)
emptyContext = iso (\txt -> (txt, Context "")) (\(txt, Context rest) -> txt <> rest)

some' :: Pprism Text a -> Ptraversal Text a
some' p = andThen' p (many' p)

many' :: Pprism Text a -> Ptraversal Text a
many' p = failing (some' p) ignored

newtype ChoicePrism a b = ChoicePrism { unChoicePrism :: Pprism a b }

-- ChoicePrism newtype for impredicative polymorphism workaround
-- Int to track which Prism succeeded, to pick the correct builder
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

-- how can second fail?
-- afb never gets invoked, t is just s
andThen' :: Pprism Text x -> Ptraversal Text x -> Ptraversal Text x
andThen' first afbsft =
  let afbsft' afb s = withPrism' first $ \build match ->
                case match s of
                  Nothing -> pure s
                  Just (a, Context unconsumed) ->
                    let fb = afb (a, Context "")
                        ft = afbsft afb (unconsumed, Context "")
                        buildFirstBefore (b, Context ctx) (s, Context ctx') = build (b, Context (ctx <> s <> ctx'))
                    in buildFirstBefore <$> fb <*> ft
  in afbsft'

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
  parse (contextualise <$> p <*> getInput) "" input
  where
    contextualise parsed unconsumed = (parsed, Context (unconsumed <> after))
    eitherToMaybe e = case e of
            Left _ -> Nothing
            Right x -> Just x
