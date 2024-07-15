{-# LANGUAGE FlexibleContexts #-}
module MarkdownToHtml where

import Types

import Lucid

import Data.Monoid
import Data.Foldable

markdownToHtml (Content xs) = div_ $ foldMap markdownItemToHtml xs

--markdownItemToHtml :: (Monad m, Term (HtmlT m ()) result) => Item String -> result
markdownItemToHtml (Markdown xs) = foldMap itemToHtml xs
markdownItemToHtml (BlockMath x) = toHtml $ "\\[" <> x <> "\\]"
markdownItemToHtml (TikzDiagram x) = toHtml $ "[$$]" <> x <> "[/$$]"

itemToHtml (Basic x) = inlineToHtml x
itemToHtml (Newline _) = br_ []
itemToHtml (MarkdownBullets b) = bulletsToHtml b
itemToHtml (Header 1 xs) = h1_ (foldMap inlineToHtml xs)
itemToHtml (Header 2 xs) = h2_ (foldMap inlineToHtml xs)
itemToHtml (Header 3 xs) = h3_ (foldMap inlineToHtml xs)
itemToHtml (Header 4 xs) = h4_ (foldMap inlineToHtml xs)
itemToHtml (Header 5 xs) = h5_ (foldMap inlineToHtml xs)
itemToHtml (Header 6 xs) = h6_ (foldMap inlineToHtml xs)

bulletsToHtml (Bullets bs) = ul_ (traverse_ bulletItemToHtml bs)
bulletItemToHtml (BulletLeaf xs) = li_ (traverse_ inlineToHtml xs)
bulletItemToHtml (BulletRecurse b) = bulletsToHtml b

inlineToHtml (BasicInline x) = baseToHtml x
inlineToHtml (Italic xs) = i_ . traverse_ baseToHtml $ xs
inlineToHtml (Bold xs) = b_ . traverse_ baseToHtml $ xs
inlineToHtml (Highlight xs) = b_ . traverse_ baseToHtml $ xs
inlineToHtml (Link xs) = traverse_ baseToHtml xs

baseToHtml (Unmarked x) = toHtml x
baseToHtml (InlineMath x) = toHtml $ "\\(" <> x <> "\\)"
