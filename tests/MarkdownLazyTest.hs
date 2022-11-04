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

   describe "||>" $ do
     it "left" $ do
       flip set "_" (text . (i ||> h 2) . _1 . _Left . unItalic)
         "*i*## h2\n" `shouldBe`
         "*_*## h2\n"

     it "right" $ do
       flip set "_" (text . (i ||> h 2) . _1 . _Right . content)
         "*i*## h2\n" `shouldBe`
         "*i*## _\n"

     it "left fails" $ do
       flip set "_" (text . (i ||> h 2) . _1 . _Left . unItalic)
         "not i## h2\n" `shouldBe`
         "not i## h2\n"

     it "right fails" $ do
       flip set "_" (text . (i ||> h 2) . _1 . _Left . unItalic)
         "*i* not header" `shouldBe`
         "*i* not header"

     describe "inside many" $ do
       it "_Left" $ do
         flip set "_" (text . many' (i ||> h 1) . _1 . _Left . unItalic)
           "*i*# h1\n*i*# h1\n" `shouldBe`
           "*_*# h1\n*_*# h1\n"

       it "_Right" $ do
         flip set "_" (text . many' (i ||> h 1) . _1 . _Right . content)
           "*i*# h1\n*i*# h1\n" `shouldBe`
           "*i*# _\n*i*# _\n"

   describe "||>?" $ do
     it "left fails" $ do
       flip set "_" (text . (i ||>? h 2) . _1 . _Left . unItalic)
         "not i## h2\n" `shouldBe`
         "not i## h2\n"

     it "right fails" $ do
       flip set "_" (text . (i ||>? h 2) . _1 . _Left . unItalic)
         "*i* not header" `shouldBe`
         "*_* not header"

   describe "many" $ do
     it "many" $ do
       flip set "_" (text . many' i . _1 . unItalic)
         "*i**i2*" `shouldBe`
         "*_**_*"

     it "empty" $ do
       flip set "_" (text . many' i . _1 . unItalic)
         "blah" `shouldBe`
         "blah"

     it "indexing into many" $ do
       flip set "_" (text . partsOf (many' i . _1) . ix 1 . unItalic)
         "*i**i2**i3*" `shouldBe`
         "*i**_**i3*"

   describe "focusing" $ do
     it "focusing" $ do
       flip set "_" (text . h 1 . focusing content . i . _1 . unItalic)
         "# *i* not i\n not h" `shouldBe`
         "# *_* not i\n not h"

     it "focusing inside ||>" $ do
       flip set "_" (text . ((h 1 . focusing content . i) ||> i) . _1 . _Left . unItalic)
         "# *i inside* not i\n*i outside*" `shouldBe`
         "# *_* not i\n*i outside*"

     it "focusing twice inside ||>" $ do
       flip set "_" (text . ((h 1 . focusing content . strikethrough . focusing unStrikethrough . i) ||> skip "") . _1 . _Left . unItalic)
         "# ~~*i* s ~~ h\n unconsumed" `shouldBe`
         "# ~~*_* s ~~ h\n unconsumed"

     it "focusing after ||>" $ do
       flip set "_" (text . (h 1 ||> i) . focusing (_Left . content) . i . _1 . unItalic)
         "# *i inside* not i\n*i outside*" `shouldBe`
         "# *_* not i\n*i outside*"

     it "focusing after many" $ do
       flip set "_" (text . many' (h 1) . focusing content . i . _1 . unItalic)
         "# *i* not i\n# *i2* not i2\n# no i\n *i* not i" `shouldBe`
         "# *_* not i\n# *_* not i2\n# no i\n *i* not i"

   describe "<||>" $ do
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
