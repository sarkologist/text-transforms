module Anki where

import Types

import Data.Semigroup

normaliseHeaders (Content cs) = Content (fmap (normaliseItem smallest) cs)
  where smallest = minHeader (Content cs)

normaliseItem smallest (Markdown items) = Markdown (fmap (normaliseHeader smallest) items)
normaliseItem _ x = x

normaliseHeader (Min n) (Header k inlines) = Header (max 1 (k-n+1)) inlines
normaliseHeader _ x = x

minHeader (Content items) = foldMap minHeaderInItem items

minHeaderInItem (Markdown items) = foldMap count items
  where
    count (Header n _) = Min n
    count _ = Min 6
minHeaderInItem _ = Min 6
