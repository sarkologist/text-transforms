{-# LANGUAGE OverloadedStrings #-}
module MarkdownLazyTest where

import Test.Tasty
import Test.Tasty.Hspec

import MarkdownLazy
import LazyParseTransforms
import Control.Lens

spec_markdown_lazy :: Spec
spec_markdown_lazy = do
 describe "markdown lazy" $ do
   it "italic" $
     flip set "_" (text . i . _1 . unItalic)
       "*i*" `shouldBe`
       "*_*"

   it "many" $ do
     flip set "_" (text . many' i . _1 . unItalic)
       "*i**i2*" `shouldBe`
       "*_**_*"

   it "indexing into many" $ do
     flip set "_" (text . partsOf (many' i . _1) . ix 1 . unItalic)
       "*i**i2**i3*" `shouldBe`
       "*i**_**i3*"

   it "focusing" $ do
     flip set "_" (text . h 1 . focusing content . i . _1 . unItalic)
       "# *i* not i\n not h" `shouldBe`
       "# *_* not i\n not h"

   it "focusing after ||>" $ do
     flip set "_" (text . (h 1 ||> i) . focusing (_Left . content) . i . _1 . unItalic)
       "# *i inside* not i\n*i outside*" `shouldBe`
       "# *_* not i\n*i outside*"

   it "focusing after many" $ do
     flip set "_" (text . many' (h 1) . focusing content . i . _1 . unItalic)
       "# *i* not i\n# *i2* not i2\n# no i\n *i* not i" `shouldBe`
       "# *_* not i\n# *_* not i2\n# no i\n *i* not i"

   it "fail <||> succeed" $ do
     flip set "_" (text . (h 1 <||> i) . _1 . _Right . unItalic)
       "*i*# h1\n" `shouldBe`
       "*_*# h1\n"

   it "succeed <||> fail" $ do
     flip set "_" (text . (h 1 <||> i) . _1 . _Left . content)
       "# h1\n*i*" `shouldBe`
       "# _\n*i*"

   it "<||> inside many: left succeeds" $ do
     flip set "_" (text . many' (i <||> h 1) . _1 . _Left . unItalic)
       "*i*# h1\n" `shouldBe`
       "*_*# h1\n"

   it "<||> inside many: right succeeds" $ do
     flip set "_" (text . many' (i <||> h 1) . _1 . _Right . content)
       "*i*# h1\n" `shouldBe`
       "*i*# _\n"
