{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LazyParseTransforms where

import Data.Text as T

import Text.Parsec hiding (choice)
import Data.Maybe
import Control.Lens hiding (Context)
import Control.Applicative

type Parser a = Parsec Text () a
newtype Context = Context Text deriving Show
type P p f a b = Optic' p f (a, Context) (b, Context)
type Pprism a b = forall p f. (Choice p, Applicative f) => P p f a b
type Ptraversal a b = forall f. (Applicative f) => P (->) f a b

focusing :: Lens' s Text -> Lens' (s, Context) (Text, Context)
focusing focus = alongside (focus . text) id . _1

text :: Iso' Text (Text, Context)
text = iso (\txt -> (txt, Context "")) (\(txt, Context rest) -> txt <> rest)

some' :: Ptraversal Text a -> Ptraversal Text a
some' p = (p ||> many' p) . alongside chosen id

many' :: Ptraversal Text a -> Ptraversal Text a
many' p = failing (some' p) ignored

newtype ChoiceTraversal a b = ChoiceTraversal { unChoiceTraversal :: Ptraversal a b }

choice' :: [ChoiceTraversal a b] -> Ptraversal a b
choice' (ChoiceTraversal p:ps) = failing p (choice' ps)
choice' [] = ignored

newtype ChoicePrism a b = ChoicePrism { unChoicePrism :: Pprism a b }

-- unlike `ignored` supports different types
-- problem: double running of afbst
(<||>) :: Ptraversal a x -> Ptraversal a y -> Ptraversal a (Either x y)
(<||>) afbst afbst' afb'' s =
  case preview afbst s of
    Just _ -> afbst afb s
    Nothing -> afbst' afb' s
  where afb  (a,ctx) = onlyIfLeft a ctx <$> afb'' (Left a, ctx)
        afb' (a,ctx) = onlyIfRight a ctx <$> afb'' (Right a, ctx)

        onlyIfRight _ _ (Right b, ctx') = (b, ctx')
        onlyIfRight a ctx (Left _, _) = (a, ctx)

        onlyIfLeft _ _ (Left b, ctx') = (b, ctx')
        onlyIfLeft a ctx (Right _, _) = (a, ctx)

-- problem: double running of afbst
(||>) :: Ptraversal a x -> Ptraversal Text y -> Ptraversal a (Either x y)
(||>) afbsft afbsft' afb'' s =
  case lastOf afbsft s of
   Just (_, Context unconsumed) ->
     let merge (a, Context rebuilt) (txt, Context ctx) = (a, Context (fromMaybe (error "unconsumed was consumed") (stripSuffix unconsumed rebuilt) <> txt <> ctx))
         ft' = afbsft' afb' (unconsumed, Context "")
     in merge <$> afbsft afb s <*> ft'
   Nothing -> pure s

  where afb  (a,ctx) = onlyIfLeft a ctx <$> afb'' (Left a, ctx)
        afb' (a,ctx) = onlyIfRight a ctx <$> afb'' (Right a, ctx)

        onlyIfRight _ _ (Right b, ctx') = (b, ctx')
        onlyIfRight a ctx (Left _, _) = (a, ctx)

        onlyIfLeft _ _ (Left b, ctx') = (b, ctx')
        onlyIfLeft a ctx (Right _, _) = (a, ctx)

parseInContext :: Parser a -> (Text, Context) -> Maybe (a, Context)
parseInContext p (input, (Context after)) = eitherToMaybe $
  parse (contextualise <$> p <*> getInput) "" input
  where
    contextualise parsed unconsumed = (parsed, Context (unconsumed <> after))
    eitherToMaybe e = case e of
            Left _ -> Nothing
            Right x -> Just x
