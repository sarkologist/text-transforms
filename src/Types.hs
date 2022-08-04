module Types where

data Content a = Content [Item a] deriving (Eq, Show)
data Item a = Markdown [MarkdownItem a] | BlockMath a deriving (Eq, Show)
data MarkdownItem a =
    Basic (Inline a)
  | Newline a
  | MarkdownBullets (Bullets a)
  | Header Int [ Inline a ]
  deriving (Eq, Show)

data Inline a =
    Unmarked a
  | Italic a
  | Bold a
  | InlineMath a
  deriving (Eq, Show)

data Bullets a = Bullets [ BulletItem a ]
  deriving (Eq, Show)

data BulletItem a =
    BulletLeaf [ Inline a ]
  | BulletRecurse (Bullets a)
  deriving (Eq, Show)
