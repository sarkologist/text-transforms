{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module LazyParseTransforms where

import Data.Text as T

import Text.Parsec
import Data.Maybe
import Data.Monoid
import Data.Functor.Product
import Control.Lens hiding (Context)
import Control.Applicative

type Parser a = Parsec Text () a
newtype Context = Context Text deriving Show
type P p f a b = Optic' p f (a, Context) (b, Context)
type Pprism a b = forall p f. (Choice p, Applicative f) => P p f a b
type Ptraversal a b = forall f. (Applicative f) => P (->) f a b

focusing :: Traversal' s Text -> Ptraversal s Text
focusing focus = _1 . focus . text

text :: Iso' Text (Text, Context)
text = iso (\txt -> (txt, Context "")) (\(txt, Context rest) -> txt <> rest)

some' :: Ptraversal Text a -> Ptraversal Text a
some' p = (p ||>? many' p) . alongside chosen id

many' :: Ptraversal Text a -> Ptraversal Text a
many' p = failing (some' p) ignored

newtype ChoiceTraversal a b = ChoiceTraversal { unChoiceTraversal :: Ptraversal a b }

choice' :: [ChoiceTraversal a b] -> Ptraversal a b
choice' (ChoiceTraversal p:ps) = failing p (choice' ps)
choice' [] = ignored

-- unlike `ignored` supports different types
(<||>) :: Ptraversal a x -> Ptraversal a y -> Ptraversal a (Either x y)
(<||>) afbst afbst' afb'' s =
  let Pair constt ft = afbst aConstfb s
  in case getConst constt of
       Any True -> ft
       Any False -> afbst' afb' s
  where aConstfb  (a,ctx) = onlyIfLeft a ctx <$> Pair (Const (Any True)) (afb'' (Left a, ctx))
        afb' (a,ctx) = onlyIfRight a ctx <$> afb'' (Right a, ctx)

(||>), (||>?) :: Ptraversal a x -> Ptraversal Text y -> Ptraversal a (Either x y)
(||>) = andThen True
(||>?) = andThen False

andThen :: Bool -> Ptraversal a x -> Ptraversal Text y -> Ptraversal a (Either x y)
andThen rightMustSucceed afbsft afbsft' afb'' s =
  let Pair constt ft = afbsft aConstfb s
  in case getLast (getConst constt) of
       Just (_, Context unconsumed) ->
         let Pair constt' ft' = afbsft' aConstfb' (unconsumed, Context "")
         in case getConst constt' of
           Any True ->
             let merge (a, Context rebuilt) (txt, Context ctx) = (a, Context (actuallyConsumed rebuilt <> txt <> ctx))
                 actuallyConsumed rebuilt | rebuilt == "" = unconsumed
                 actuallyConsumed rebuilt | otherwise =
                   fromMaybe (error . unpack $ "unconsumed was consumed: \"" <> unconsumed <> "\" / \"" <> rebuilt <> "\"") $
                     (stripSuffix unconsumed rebuilt)
             in merge <$> ft <*> ft'
           Any False -> if rightMustSucceed then pure s else ft
       Nothing -> pure s

  where aConstfb  (a,ctx) = onlyIfLeft a ctx <$> Pair (Const (Last (Just (a,ctx)))) (afb'' (Left a, ctx))
        aConstfb' (a,ctx) = onlyIfRight a ctx <$> Pair (Const (Any True)) (afb'' (Right a, ctx))

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
