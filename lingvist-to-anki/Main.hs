{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid

import Turtle
import Haquery


parse = parseHtml . T.pack
translation = Haquery.select ".translation"
test = Haquery.select ".context > span"

extractTestWord = alter "#card-gap[data-word]" extract
extract t = maybe t spanTag maybeWord
  where
    spanTag = tag "span" [] . (:[]) . Haquery.text
    maybeWord = attr "data-word" t

reintroduceSpaces' = alter ".after" (const spaceTag)

reintroduceSpace x | isEmptySpan x = spaceTag
reintroduceSpace x = x

spaceTag = tag "span" [] [Haquery.text " "]
isEmptySpan t = name t == "span" && innerText t == ""

extractTagText :: Tag -> T.Text
extractTagText (Tag _ _ _ cs) = foldMap extractTagText cs
extractTagText (Text _ t) = t
extractTagText _ = mempty

removeAllAttributes (Tag i n _ cs) = Tag i n (toAttrs []) cs
removeAllAttributes x = x

textOnly :: Tag -> Tag
textOnly = div' [] . (:[]) . Haquery.text . extractTagText

overChildren :: (Tag -> Tag) -> Tag -> Tag
overChildren f t = tag (name t) (M.toList (attrsToMap (attrs t))) (map f (children t))

testPart = map (overChildren reintroduceSpace . extractTestWord) . test
translationPart = map ( overChildren reintroduceSpace) . translation

getHtml = parseHtml . lineToText <$> inproc "/usr/local/bin/pbv" ["public.html"] empty
testText = div' [] . (:[]) . Haquery.text . foldMap extractTagText . concatMap testPart
translationText = concatMap (map textOnly . translationPart)

go = do
  html <- getHtml

  liftIO $ putStrLn . T.unpack . foldMap render $ [testText html] <> translationText html

main :: IO ()
main = sh go

