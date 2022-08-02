module Types where

data Content a = Content [Item a] deriving (Eq, Show)
data Item a = Markdown [MarkdownItem a] | InlineMath a | BlockMath a deriving (Eq, Show)
data MarkdownItem a =
    Basic (MarkdownItemBasic a)
  | Header Int [ MarkdownItemBasic a ]
  deriving (Eq, Show)

data MarkdownItemBasic a =
    Newline a
  | Unmarked a
  | Italic a
  | Bold a
  deriving (Eq, Show)

