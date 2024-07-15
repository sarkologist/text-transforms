module Types where

data Content a = Content [Item a] deriving (Eq, Show)
data Item a =
   Markdown [MarkdownItem a]
 | BlockMath a
 | TikzDiagram a
  deriving (Eq, Show)

data MarkdownItem a =
    Basic (Inline a)
  | Newline a
  | MarkdownBullets (Bullets a)
  | Header Int [ Inline a ]
  deriving (Eq, Show)

data Inline a =
    BasicInline (Base a)
  | Italic [ Base a ]
  | Highlight [ Base a ]
  | Bold [ Base a ]
  | Link [ Base a ]
  deriving (Eq, Show)

data Base a =
    Unmarked a
  | InlineMath a
  deriving (Eq, Show)

data Bullets a = Bullets [ BulletItem a ]
  deriving (Eq, Show)

data BulletItem a =
    BulletLeaf [ Inline a ]
  | BulletRecurse (Bullets a)
  deriving (Eq, Show)
