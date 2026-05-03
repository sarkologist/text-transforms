{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module MarkdownToHtml where

import Types

import Lucid

import Data.Monoid
import Data.Foldable
import Data.Char (toUpper)
import Data.List (intercalate)
import qualified Data.Text as T

markdownToHtml (Content xs) = div_ $ foldMap markdownItemToHtml xs

--markdownItemToHtml :: (Monad m, Term (HtmlT m ()) result) => Item String -> result
markdownItemToHtml (Markdown xs) = foldMap itemToHtml $ cleanUp xs
markdownItemToHtml (BlockMath x) = toHtml $ "\\[" <> x <> "\\]"
markdownItemToHtml (TikzDiagram x) = toHtml $ "[$$]" <> x <> "[/$$]"

cleanUp (Newline x: Basic (Tag _): Newline _ : xs) = Newline x : cleanUp xs
cleanUp (header@(Header _ _): Basic (Tag _): Newline _ : xs) = header : cleanUp xs
cleanUp (Basic (Tag _) : xs) = cleanUp xs
cleanUp (x:xs) = x : cleanUp xs
cleanUp [] = []

itemToHtml (Basic x) = inlineToHtml x
itemToHtml (Blockquote xs) = blockquote_ $ foldMap itemToHtml xs
itemToHtml (Callout kind title xs) =
  aside_ [class_ (T.pack (calloutClasses kind))] $ do
    div_ [class_ "callout-title"] (calloutTitle kind title)
    div_ [class_ "callout-content"] (foldMap itemToHtml (cleanUp xs))
itemToHtml (Newline _) = br_ []
itemToHtml (MarkdownBullets b) = bulletsToHtml b
itemToHtml (MarkdownBlockMath x) = toHtml $ "\\[" <> x <> "\\]"
itemToHtml (MarkdownTable t) = tableToHtml t
itemToHtml (Header 1 xs) = h1_ (foldMap inlineToHtml xs)
itemToHtml (Header 2 xs) = h2_ (foldMap inlineToHtml xs)
itemToHtml (Header 3 xs) = h3_ (foldMap inlineToHtml xs)
itemToHtml (Header 4 xs) = h4_ (foldMap inlineToHtml xs)
itemToHtml (Header 5 xs) = h5_ (foldMap inlineToHtml xs)
itemToHtml (Header 6 xs) = h6_ (foldMap inlineToHtml xs)

bulletsToHtml (Bullets UnorderedList bs) = ul_ (traverse_ bulletItemToHtml bs)
bulletsToHtml (Bullets OrderedList bs) = ol_ (traverse_ bulletItemToHtml bs)
bulletItemToHtml (BulletLeaf xs) = li_ (foldMap itemToHtml (cleanUp xs))
bulletItemToHtml (BulletRecurse b) = bulletsToHtml b

tableToHtml :: Monad m => Table String -> HtmlT m ()
tableToHtml (Table headerRows bodyRows) =
  table_ $ do
    thead_ . tr_ $ traverse_ (th_ . traverse_ inlineToHtml) headerRows
    tbody_ $ traverse_ tableRowToHtml bodyRows

tableRowToHtml :: Monad m => [[Inline String]] -> HtmlT m ()
tableRowToHtml cells = tr_ $ traverse_ (td_ . traverse_ inlineToHtml) cells

inlineToHtml (BasicInline x) = baseToHtml x
inlineToHtml (Italic xs) = i_ . traverse_ baseToHtml $ xs
inlineToHtml (Bold xs) = b_ . traverse_ baseToHtml $ xs
inlineToHtml (Highlight xs) = b_ . traverse_ baseToHtml $ xs
inlineToHtml (Link xs) = b_ . traverse_ baseToHtml $ xs
inlineToHtml (Tag _) = pure ()

baseToHtml (Unmarked x) = toHtml x
baseToHtml (InlineMath x) = toHtml $ "\\(" <> x <> "\\)"

calloutClasses :: String -> String
calloutClasses kind = "callout callout-" <> kind

calloutTitle :: Monad m => String -> [Inline String] -> HtmlT m ()
calloutTitle kind [] = toHtml (fallbackCalloutTitle kind)
calloutTitle _ title = foldMap inlineToHtml title

fallbackCalloutTitle :: String -> String
fallbackCalloutTitle = intercalate " " . fmap capitalize . splitOn '-'
  where
    capitalize "" = ""
    capitalize (x:xs) = toUpper x : xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter = foldr split [[]]
  where
    split x acc@(part:parts)
      | x == delimiter = [] : acc
      | otherwise = (x:part) : parts
    split _ [] = []
