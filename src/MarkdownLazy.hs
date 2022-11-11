{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MarkdownLazy where

import MarkdownParse (withinMany)

import LazyParseTransforms

import Data.Text as T
import Text.Parsec hiding (choice)
import Control.Lens hiding (Context, noneOf)
import Control.Lens.TH
import Control.Monad.State as S

newtype Italic = Italic { _unItalic :: Text } deriving Show
newtype Strikethrough = Strikethrough { _unStrikethrough :: Text } deriving Show
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

data Cases =
    Bullet Text Int Text
  | HeaderTitleContent Int Text Text
  | Plain Text
  deriving (Show)
makePrisms ''Cases

buildCases :: Cases -> Text
buildCases (Plain txt) = txt
buildCases (Bullet style lvl txt) = T.replicate lvl style <> "- " <> txt <> "\n"
buildCases (HeaderTitleContent lvl title content) = T.replicate lvl "#" <> " " <> title <> "\n" <> content

bullet :: Pprism Text Cases
bullet = prism' build match
  where
    match = parseInContext $ uncurry Bullet <$> indentation <*> content

    build (md, ctx) = (buildCases md, ctx)

    content = pack <$> many1 (noneOf "\n") <* char '\n'

    indentation :: Parser (Text, Int)
    indentation = (styleLengthOf "  " <|> styleLengthOf "\t" <|> noIndent) <* string "- "

    styleLengthOf :: String -> Parser (Text, Int)
    styleLengthOf x = (\len -> (pack x, len)) . Prelude.length <$> many1 (string x)

    noIndent = ("", 0) <$ string ""

htc :: Pprism Text Cases
htc = prism' build match
  where
    match = parseInContext $
          (HeaderTitleContent <$> (numHashes <* char ' '))
      <*> title
      <*> content
    build (md, ctx) = (buildCases md, ctx)

    numHashes = Prelude.length <$> many1 (char '#')
    title = pack <$> many1 (noneOf ['\n']) <* char '\n'
    content = (pack.) . (<>) <$> manyTill anyChar (() <$ try (lookAhead (string "\n#")) <|> eof) <*> (string "\n" <|> "" <$ eof)

unindentBulletIntoSubheader :: Text -> Text -> Text
unindentBulletIntoSubheader style = execState $
  zoom (text . many' htc . _1 . _HeaderTitleContent) $ do
    (headerLevel, _, _) <- get
    zoom (_3 . text . many' bullet . _1) $ do
       let f (Bullet _ lvl content) = (if lvl==0 then (HeaderTitleContent (headerLevel+1) content "") else Bullet style (lvl-1) content)
       modify f


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

makeLenses ''Italic
makeLenses ''Header
makeLenses ''Strikethrough
makePrisms ''Header
makePrisms ''Italic
makePrisms ''Strikethrough
