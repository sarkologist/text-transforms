{-# LANGUAGE FlexibleContexts #-}
module MarkdownToHtml where

import Types

import Lucid

import Data.Monoid
import Data.Foldable

markdownToHtml (Content xs) = div_ $ foldMap markdownItemToHtml xs

--markdownItemToHtml :: (Monad m, Term (HtmlT m ()) result) => Item String -> result
markdownItemToHtml (Markdown xs) = foldMap itemToHtml xs
markdownItemToHtml (InlineMath x) = toHtml $ "\\(" <> x <> "\\)"
markdownItemToHtml (BlockMath x) = toHtml $ "\\[" <> x <> "\\]"

itemToHtml (Basic x) = basicToHtml x
itemToHtml (Newline _) = br_ []
itemToHtml (MarkdownBullets b) = bulletsToHtml b
itemToHtml (Header 1 xs) = h1_ (foldMap basicToHtml xs)
itemToHtml (Header 2 xs) = h2_ (foldMap basicToHtml xs)
itemToHtml (Header 3 xs) = h3_ (foldMap basicToHtml xs)
itemToHtml (Header 4 xs) = h4_ (foldMap basicToHtml xs)
itemToHtml (Header 5 xs) = h5_ (foldMap basicToHtml xs)
itemToHtml (Header 6 xs) = h6_ (foldMap basicToHtml xs)

bulletsToHtml (Bullets bs) = ul_ (traverse_ bulletItemToHtml bs)
bulletItemToHtml (BulletLeaf xs) = li_ (traverse_ basicToHtml xs)
bulletItemToHtml (BulletRecurse b) = bulletsToHtml b

basicToHtml (Unmarked x) = toHtml x
basicToHtml (Italic x) = i_ $ toHtml x
basicToHtml (Bold x) = b_ $ toHtml x
