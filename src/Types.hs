module Types where

data Content a = Content [Item a] deriving (Eq, Show)
data Item a = Markdown [MarkdownItem a] | InlineMath a | BlockMath a deriving (Eq, Show)
data MarkdownItem a =
    Basic (MarkdownItemBasic a)
  | Newline a
  | MarkdownBullets (Bullets a)
  | Header Int [ MarkdownItemBasic a ]
  deriving (Eq, Show)

data MarkdownItemBasic a =
    Unmarked a
  | Italic a
  | Bold a
  deriving (Eq, Show)

data Bullets a = Bullets [ BulletItem a ]
  deriving (Eq, Show)

data BulletItem a =
    BulletLeaf [ MarkdownItemBasic a ]
  | BulletRecurse (Bullets a)
  deriving (Eq, Show)
