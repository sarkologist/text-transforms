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
     flip set "x" (text . i . _1 . unItalic)
       "*i*" `shouldBe`
       "*x*"

   it "many" $ do
     flip set "x" (text . many' i . _1 . unItalic)
       "*i**i2*" `shouldBe`
       "*x**x*"

   it "indexing into many" $ do
     flip set "x" (text . partsOf (many' i . _1) . ix 1 . unItalic)
       "*i**i2**i3*" `shouldBe`
       "*i**x**i3*"

   it "focusing" $ do
     flip set "x" (text . h 1 . focusing content . i . _1 . unItalic)
       "# *i* not i\n not h" `shouldBe`
       "# *x* not i\n not h"

   it "focusing inside many" $ do
     flip set "x" (text . many' (h 1 . focusing content . i) . _1 . unItalic)
       "# *i* not i\n# *i2* not i2\n# no i\n *i* not i" `shouldBe`
       "# *x* not i\n# *x* not i2\n# no i\n *i* not i"

   it "focusing outside many" $ do
     flip set "x" (text . many' (h 1) . focusing content . i . _1 . unItalic)
       "# *i* not i\n# *i2* not i2\n# no i\n *i* not i" `shouldBe`
       "# *x* not i\n# *x* not i2\n# no i\n *i* not i"

   it "fail <||> succeed" $ do
     flip set "x" (text . (h 1 <||> i) . _1 . _Right . unItalic)
       "*i*# h1\n" `shouldBe` "*x*# h1\n"

   it "succeed <||> fail" $ do
     flip set "x" (text . (h 1 <||> i) . _1 . _Left . content)
       "# h1\n*i*" `shouldBe` "# x\n*i*"

   it "<||> inside many: left succeeds" $ do
     flip set "x" (text . many' (i <||> h 1) . _1 . _Left . unItalic)
       "*i*# h1\n" `shouldBe` "*x*# h1\n"

   it "<||> inside many: right succeeds" $ do
     flip set "x" (text . many' (i <||> h 1) . _1 . _Right . content)
       "*i*# h1\n" `shouldBe` "*i*# x\n"
