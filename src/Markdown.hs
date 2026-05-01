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
unindentBullets (Bullets _ bs) = concat $ onitems . unbullet <$> bs
  where
    onitems (Left is) = is
    onitems (Right b) = [ MarkdownBullets b ]

unbullet :: BulletItem a -> Either [MarkdownItem a] (Bullets a)
unbullet (BulletLeaf is) = Left is
unbullet (BulletRecurse bs) = Right bs
