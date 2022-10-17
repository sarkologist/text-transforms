module Markdown where

import Types

unindentAll (Content is) = Content (onItems <$> is)
  where
    onItems (Markdown is) = Markdown (is >>= unindent)
    onItems x = x

unindent :: MarkdownItem a -> [ MarkdownItem a ]
unindent (Header k xs) | k > 1 = [ Header (k-1) xs ]
unindent (MarkdownBullets b) = unindentBullets b
unindent x = [x]


unindentBullets :: Bullets a -> [ MarkdownItem a ]
unindentBullets (Bullets bs) = concat $ onitems . unbullet <$> bs
  where
    onitems (Left is) = Basic <$> is
    onitems (Right (Bullets bs)) = [ MarkdownBullets (Bullets bs) ]

unbullet :: BulletItem a -> Either [Inline a] (Bullets a)
unbullet (BulletLeaf is) = Left is
unbullet (BulletRecurse bs) = Right bs
