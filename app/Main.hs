{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as T
import Data.Monoid

import Control.Lens (ix, only, filteredBy, deep, traversed, Traversal',
                    preview, zoom, set, (.=))
import Control.Monad.State
import Data.Maybe


import Text.Taggy (Node)
import Text.Taggy.Lens
import Text.Taggy.Renderer

import Turtle

mathElements :: Traversal' Node Element
mathElements = elements . deep (filteredBy (attributed (ix "class" . only "mwe-math-element")))

latexInMathElement :: Traversal' Element T.Text
latexInMathElement = children . traversed .  elements . deep (named (only "annotation")) . contents

replaceLatex :: State Node ()
replaceLatex =
  zoom mathElements $ do
    latex <- zoom latexInMathElement get
    children .= [NodeContent $ "$$" <> latex <> "$$"]


getHtml = LT.fromStrict . ensureSingleTopLevelTag . lineToText <$> inproc "/usr/local/bin/pbv" ["public.html"] empty
getText = LT.fromStrict . ensureSingleTopLevelTag . lineToText <$> inproc "pbpaste" [] empty

ensureSingleTopLevelTag s = "<html>" <> s <> "</html>"

go = do
  source <- getHtml
  let replaced = execState replaceLatex (fromJust (preview html source))
  liftIO . LTIO.putStrLn . render $ replaced

main :: IO ()
main = sh go
