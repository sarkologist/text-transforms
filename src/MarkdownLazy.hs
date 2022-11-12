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

i :: PPrism Text Italic
i = pPrism parse render
  where
    parse = Italic . pack <$> withinMany (char '*') (noneOf (['*']))
    render (Italic txt) = ("*" <> txt <> "*")

strikethrough :: PPrism Text Strikethrough
strikethrough = pPrism parse render
  where
    parse = Strikethrough . pack <$> withinMany (string "~~") (noneOf (['~']))
    render (Strikethrough txt) = ("~~" <> txt <> "~~")

h :: Int ->  PPrism Text Header
h n = pPrism parse render
  where
    parse = Header n . pack <$> between (string (hashes n) *> char ' ') endOfLine (many1 (noneOf ['\n']))
    render (Header k txt) = pack (hashes k) <> " " <> txt <> "\n"

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

bullet :: PPrism Text Cases
bullet = pPrism parse render
  where
    parse = uncurry Bullet <$> indentation <*> content

    render = buildCases

    content = pack <$> many1 (noneOf "\n") <* char '\n'

    indentation :: Parser (Text, Int)
    indentation = (styleLengthOf "  " <|> styleLengthOf "\t" <|> noIndent) <* string "- "

    styleLengthOf :: String -> Parser (Text, Int)
    styleLengthOf x = (\len -> (pack x, len)) . Prelude.length <$> many1 (string x)

    noIndent = ("", 0) <$ string ""

htc :: PPrism Text Cases
htc = pPrism parse render
  where
    parse = (HeaderTitleContent <$> (numHashes <* char ' '))
      <*> title
      <*> content
    render = buildCases

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


headers :: PTraversal Text Header
headers = choice' [
    ChoiceTraversal (h 1)
  , ChoiceTraversal (h 2)
  , ChoiceTraversal (h 3)
  , ChoiceTraversal (h 4)
  , ChoiceTraversal (h 5)
  , ChoiceTraversal (h 6)
  ]

skip :: [Char] -> PPrism Text Text
skip toSkip = pPrism parse id
  where
    parse = pack <$> many1 (noneOf toSkip)

allTheHeaders :: PTraversal Text (Either Header Text)
allTheHeaders = many' (headers <||> skip "#")

makeLenses ''Italic
makeLenses ''Header
makeLenses ''Strikethrough
makePrisms ''Header
makePrisms ''Italic
makePrisms ''Strikethrough
