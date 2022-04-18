{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as T

import Control.Monad.State
import Control.Applicative
import qualified Control.Foldl as F
import Data.Maybe
import Data.Monoid
import Data.List

import Data.Attoparsec.Text       as Atto
import Lucid

import Turtle as T (stdin, lineToText, inproc, die, empty, fold)

getText = T.fold stdin (F.foldMap (\l -> lineToText l <> "\n") id)
getTextFromClipboard :: IO T.Text
getTextFromClipboard = T.fold (inproc "pbpaste" [] empty) (F.foldMap (\l -> lineToText l <> "\n") id)

data Content a = Line [LineElement a] | Math [a] deriving Show
data LineElement a = TextElement a | InlineMath a deriving Show

many1Till p x = (:) <$> p <*> manyTill p x

pureLineContent = manyTill (notChar '$') (char '\n')
lineContentWithMath = (:) <$> fmap TextElement (many1Till (notChar '$') (char '$')) <*> inlineMathContents
inlineMath = char '$' *> inlineMathContents
inlineMathContents = ((:[]) . InlineMath) <$> many1Till (notChar '$') (char '$')

line = fmap mconcat (many1Till l endOfLine <|> many l)
  where l = (lineContentWithMath <|> inlineMath) <|> fmap ((:[]) . TextElement) pureLineContent

blockMathLine = pureLineContent <* endOfLine <|> pureLineContent

blockMath = do
  string "$$"
  endOfLine
  manyTill blockMathLine (string "$$")

contents = manyTill (fmap Math blockMath <|> fmap Line line) endOfInput

asHtml :: (Monad m, Term (HtmlT m ()) result) => Content String -> result
asHtml (Math ls) = li_ $ pre_ $ do
  "$$"
  sequence (intersperse (br_ []) . map toHtml $ ls)
  "$$"
asHtml (Line l) = li_ (toHtml . foldMap convert $ l)
  where
    convert (TextElement e) = e
    convert (InlineMath e) = "$$" <> e <> "$$"

formatList :: (Monad m, Term (HtmlT m ()) result) => [Content String] -> result
formatList ls = ul_ (mapM_ asHtml ls)


go = do
  source <- getText
  case parseOnly contents source of
    Left err -> die (T.pack err)
    Right result -> do
      LTIO.putStrLn . renderText . formatList $ result

main :: IO ()
main = go
