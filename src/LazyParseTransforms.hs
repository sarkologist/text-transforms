{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module LazyParseTransforms where

import Data.Text as T
import Data.Vector as V

import Text.Parsec
import Data.Maybe
import Data.Monoid
import Data.Functor.Product
import Control.Lens hiding (Context)
import Control.Applicative

type Parser a = Parsec Text () a
-- first is unconsumed
-- second is list of unconsumeds after successive `focusing`
-- which we need to preserve for ||> to know what is unconsumed at the level it is applied
data Context = Context Text (Vector Text) Int deriving Show
type P p f a b = Optic' p f (a, Context) (b, Context)
type Pprism a b = forall p f. (Choice p, Applicative f) => P p f a b
type Ptraversal a b = forall f. (Applicative f) => P (->) f a b

-- "vertical" top-down composition:
-- prepare new Context for text at `focus`
-- save current-level unconsumed to `Context`
focusing :: Traversal' s Text -> Ptraversal Text a -> Ptraversal s a
focusing focus inside afb s@(_, ctx@(Context unconsumed above lvl)) =
  let outside_afbsft = _1 . focus . textAtLevel (lvl+1) (V.snoc above unconsumed) . inside
  in outside_afbsft afb s

textAtLevel :: Int -> Vector Text -> Iso' Text (Text, Context)
textAtLevel lvl unconsumeds = iso (\txt -> (txt, Context "" unconsumeds lvl)) (\(txt, Context rest _ _) -> txt <> rest)

text :: Iso' Text (Text, Context)
text = textAtLevel 0 V.empty

many' :: Ptraversal Text a -> Ptraversal Text a
many' p = failing (some' p) ignored
  where
    some' :: Ptraversal Text a -> Ptraversal Text a
    some' p = (p ||>? many' p) . alongside chosen id

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

-- "horizontal" left-to-right composition:
-- does not change level, just current-level unconsumed
-- first crux is passing on remaining unconsumed after left has succeeded
-- second crux is left may be focused,
--   in which case we need to retrieve its parent unconsumed
-- third crux is after left context has been rebuilt,
--   we need to trim it because unconsumed should now be to the after right, not left
andThen :: Bool -> Ptraversal a x -> Ptraversal Text y -> Ptraversal a (Either x y)
andThen rightMustSucceed afbsft afbsft' afb'' s@(_, Context _ above lvl_s) =
  let Pair constt ft = afbsft aConstfb s
  in case getConst constt of
       Last (Just (unconsumed, isFocused)) ->
         let Pair constt' ft' = afbsft' aConstfb' (unconsumed, Context "" above lvl_s)
         in case getConst constt' of
           Any True ->
             let merge (a, Context ctx _ _) (txt, Context ctx' _ _) =
                   -- if focused, then 'a' is in fact 'Text' and will be rebuilt entirely, so discard 'ctx'
                   -- if not focused, we have already discarded unconsumed, so just use rebuilt 'ctx'
                   let rebuilt = if isFocused then "" else ctx
                   in (a, Context (rebuilt <> txt <> ctx') above lvl_s)
             in merge <$> ft <*> ft'
           Any False -> if rightMustSucceed
                        then pure s
                        else let replaceUnconsumed (t, Context rebuilt abv l) =
                                   (t, Context (rebuilt <> unconsumed) abv l)
                             in replaceUnconsumed <$> ft
       Last Nothing -> pure s

  where aConstfb  (a,ctx@(Context unconsumed abv lvl)) =
          let isFocused = lvl > 0
              -- discard 'unconsumed' if focused, since it is now the responsibility of afbsft'
              -- otherwise 'unconsumed' is local to afbst, so will be rebuilt into ancestors
              ctx' = Context (if isFocused then unconsumed else "") abv lvl
              unconsumed_top = fromMaybe unconsumed (abv !? lvl_s)
          in onlyIfLeft a ctx' <$> Pair
              (Const (Last (Just (unconsumed_top, isFocused))))
              (afb'' (Left a, ctx'))
        aConstfb' (a,ctx) = onlyIfRight a ctx <$> Pair (Const (Any True)) (afb'' (Right a, ctx))

onlyIfRight _ _ (Right b, ctx') = (b, ctx')
onlyIfRight a ctx (Left _, _) = (a, ctx)

onlyIfLeft _ _ (Left b, ctx') = (b, ctx')
onlyIfLeft a ctx (Right _, _) = (a, ctx)

pprism :: Parser a -> (a -> Text) -> Pprism Text a
pprism parse render = prism' build match
  where
    match = parseInContext parse
    build (a, ctx) = (render a, ctx)

parseInContext :: Parser a -> (Text, Context) -> Maybe (a, Context)
parseInContext p (input, (Context after above lvl)) = eitherToMaybe $
  parse (contextualise <$> p <*> getInput) "" input
  where
    contextualise parsed unconsumed = (parsed, Context (unconsumed <> after) above lvl)
    eitherToMaybe e = case e of
            Left _ -> Nothing
            Right x -> Just x
