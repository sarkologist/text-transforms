{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module MarkdownLazy where

import MarkdownParse (withinMany)

import LazyParseTransforms

import Data.Text as T
import Text.Parsec hiding (choice)
import Control.Lens hiding (Context, noneOf)
import Control.Lens.TH
import Data.Fix
import Text.Show.Deriving
import Data.Functor.Classes
import Control.Monad.State as S

newtype Italic = Italic { _unItalic :: Text } deriving Show
newtype Strikethrough = Strikethrough { _unStrikethrough :: Text } deriving Show
data Bullet = Bullet Int Text deriving Show
data Header = Header {
  _level :: Int,
  _content :: Text
} deriving Show

i :: Pprism Text Italic
i = prism' build match
  where
    match = parseInContext $ Italic . pack <$> withinMany (char '*') (noneOf (['*']))
    build (Italic txt, ctx) = ("*" <> txt <> "*", ctx)

strikethrough :: Pprism Text Strikethrough
strikethrough = prism' build match
  where
    match = parseInContext $ Strikethrough . pack <$> withinMany (string "~~") (noneOf (['~']))
    build (Strikethrough txt, ctx) = ("~~" <> txt <> "~~", ctx)

h :: Int ->  Pprism Text Header
h n = prism' build match
  where
    match = parseInContext $ Header n . pack <$> between (string (hashes n) *> char ' ') endOfLine (many1 (noneOf ['\n']))
    build (Header k txt, ctx) = (pack (hashes k) <> " " <> txt <> "\n" , ctx)

    hashes k = Prelude.take k (Prelude.repeat '#')

data Cases a =
    B Int a
  | H Int a
  | HeaderTitleContent Int a a
  | Plain Text
  deriving (Show, Functor)
$(deriveShow1 ''Cases)
makePrisms ''Cases

type Markdown = Fix Cases

buildCases :: Cases Text -> Text
buildCases (Plain txt) = txt
buildCases (B lvl txt) = T.replicate lvl "  " <> "- " <> txt <> "\n"
buildCases (H lvl txt) = T.replicate lvl "#" <> " " <> txt <> "\n"
buildCases (HeaderTitleContent lvl title content) = T.replicate lvl "#" <> " " <> title <> "\n" <> content

buildMarkdown = foldFix buildCases

bullet :: Pprism Text Markdown
bullet = prism' build match
  where
    match = parseInContext $ Fix <$> ((B <$> depth) <*> (Fix . Plain <$> content))
    depth = spaces <* string "- "
    content = pack <$> many1 (noneOf "\n") <* char '\n'

    build (md, ctx) = (buildMarkdown md, ctx)

    spaces :: Parser Int
    spaces =  Prelude.length <$> many (string "  ")

hMarkdown :: Pprism Text Markdown
hMarkdown = prism' build match
  where
    match = parseInContext $ Fix <$> ((H <$> (numHashes <* char ' ')) <*> (Fix . Plain <$> content))
    build (md, ctx) = (buildMarkdown md, ctx)

    numHashes = Prelude.length <$> many1 (char '#')
    content = pack <$> many1 (noneOf ['\n']) <* char '\n'

htc :: Pprism Text Markdown
htc = prism' build match
  where
    match = parseInContext $ Fix
      <$> ((HeaderTitleContent <$> (numHashes <* char ' '))
      <*> (Fix . Plain <$> title)
      <*> (Fix . Plain <$> content))
    build (md, ctx) = (buildMarkdown md, ctx)

    numHashes = Prelude.length <$> many1 (char '#')
    title = pack <$> many1 (noneOf ['\n']) <* char '\n'
    content = (pack.) . (<>) <$> manyTill anyChar (() <$ try (lookAhead (string "\n#")) <|> eof) <*> (string "\n" <|> "" <$ eof)

unindentBulletIntoSubheader :: Text -> Text
unindentBulletIntoSubheader = execState $
  zoom (text . many' htc . _1 . _Fix. _HeaderTitleContent) $ do
    (headerLevel, _, _) <- get
    zoom (_3 . _Fix . _Plain) $ do
       let f (Fix (B lvl content)) = Fix (if lvl==0 then (H (headerLevel+1) content) else B (lvl-1) content)
       modify $ over (text . many' bullet . _1) f


headers :: Ptraversal Text Header
headers = choice' [
    ChoiceTraversal (h 1)
  , ChoiceTraversal (h 2)
  , ChoiceTraversal (h 3)
  , ChoiceTraversal (h 4)
  , ChoiceTraversal (h 5)
  , ChoiceTraversal (h 6)
  ]

skip :: [Char] -> Pprism Text Text
skip toSkip = prism' id match
  where
    match = parseInContext $ pack <$> many1 (noneOf toSkip)

allTheHeaders :: Ptraversal Text (Either Header Text)
allTheHeaders = many' (headers <||> skip "#")

_Fix :: Iso' (Fix f) (f (Fix f))
_Fix = iso unFix Fix

makeLenses ''Italic
makeLenses ''Header
makeLenses ''Strikethrough
makePrisms ''Header
makePrisms ''Italic
makePrisms ''Strikethrough
