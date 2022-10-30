{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LazyParseTransforms where

import Data.Text as T

import Text.Parsec hiding (choice)
import Control.Lens (preview, Lens', Traversal', Iso', Choice, Optic', alongside, Prism', prism, prism', withPrism, iso, swapped, mapping, _Left, failing, ignored, _1)
import Control.Applicative

type Parser a = Parsec Text () a
data Context = Context Text [Text] deriving Show
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

skipping :: Pprism Text Text -> Pprism s a -> Pprism s a
skipping skip want =
  withPrism' want $ \b m ->
    withPrism' skip $ \b' m' ->
      let match s = case m s of
            Just original@(a, Context unconsumed skip) ->
              case m' (unconsumed, Context "" []) of
                Just (toSkip, Context unconsumed' _) -> Just (a, Context unconsumed' (toSkip:skip))
                Nothing -> Just original
            Nothing -> Nothing
          build (a, Context unconsumed (skip:rest)) = b (a, Context (skip <> unconsumed) rest)
      in prism' build match

emptyContext :: Iso' Text (Text, Context)
emptyContext = iso (\txt -> (txt, Context "" [])) (\(txt, Context rest [skip]) -> txt <> skip <> rest)

some' :: Ptraversal Text a -> Ptraversal Text a
some' p = andThen' p (many' p)

many' :: Ptraversal Text a -> Ptraversal Text a
many' p = failing (some' p) ignored

newtype ChoiceTraversal a b = ChoiceTraversal { unChoiceTraversal :: Ptraversal a b }

choice' :: [ChoiceTraversal a b] -> Ptraversal a b
choice' (ChoiceTraversal p:ps) = failing p (choice' ps)
choice' [] = ignored

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


-- takes traversal unlike andThen, but requires same target type
-- does this run the first traversal twice?
-- do we need to enforce that the first returns only 1?
andThen' :: Ptraversal a x -> Ptraversal Text x -> Ptraversal a x
andThen' afbsft afbsft' afb s =
  case preview afbsft s of
    Nothing -> pure s
    Just (a, Context unconsumed skip) ->
      let fb = afb (a, Context "" [])
          ft' = afbsft' afb (unconsumed, Context "" skip)
          ab' (b, Context ctx _) (s, Context ctx' skip') = (b, Context (ctx <> s <> ctx') skip')
          fb' =  ab' <$> fb <*> ft'
      in afbsft (const fb') s

andThen :: Pprism a x -> Pprism Text y -> Pprism a (x, y)
andThen first second = withPrism' first $ \b m ->
  let
    match (a, ctx) = case m (a, ctx) of
      Just (x, (Context unconsumed skip)) -> withPrism' second $ \b' m' ->
        case m' (unconsumed, (Context "" skip)) of
          Just (y, ctx') -> Just ((x,y), ctx')
          Nothing -> Nothing
      Nothing -> Nothing

    build ((x,y), ctx) = withPrism' second $ \b' m' ->
      case b' (y, ctx) of
         (t, Context ctx' skip') -> b (x, (Context (t <> ctx') skip'))
   in prism' build match

parseInContext :: Parser a -> (Text, Context) -> Maybe (a, Context)
parseInContext p (input, (Context after skip)) = eitherToMaybe $
  parse (contextualise <$> p <*> getInput) "" input
  where
    contextualise parsed unconsumed = (parsed, Context (unconsumed <> after) skip)
    eitherToMaybe e = case e of
            Left _ -> Nothing
            Right x -> Just                                                                                                                                                                         x
