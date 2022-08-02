{-# LANGUAGE OverloadedStrings #-}
module MarkdownToHtmlTest where

import Test.Tasty
import Test.Tasty.HUnit

import MarkdownToHtml

import Text.Parsec (parse)

test_individual :: TestTree
test_individual =
  testGroup "individual"
  [
    testCase "unmarked" $
      parse (unmarked "") "" "abc" @?= Right (Unmarked "abc")
  , testCase "unmarked only" $
      parse (unmarked "*") "" "abc*i*" @?= Right (Unmarked "abc")
  , testCase "italic" $
      parse italic "" "*abc*" @?= Right (Italic "abc")
  , testCase "bold" $
      parse bold "" "**abc**" @?= Right (Bold "abc")
  , testCase "header simple" $
      parse (header 1) "" "# abc\n" @?= Right (Header 1 [ Unmarked "abc" ])
  , testCase "header complex" $
      parse (header 1) "" "# abc **bold**\n" @?= Right (Header 1 [ Unmarked "abc ", Bold "bold" ])
  ]


test_complex =
  testGroup "complex"
  [
      testGroup "unmarked" $
      [
        testCase "non-greedy" $
          parse (unmarked "*" *> italic) "" "abc *i*" @?= Right ((Italic "i") )
      ]
    , testGroup "markdown"
    [
      testCase "unmarked" $
        parse markdown "" "abc" @?= Right (Markdown [ Basic (Unmarked "abc") ])
    , testCase "basic" $
        parse markdown "" "abc *i* **b**" @?=
          Right (Markdown [
                Basic (Unmarked "abc ")
              , Basic (Italic "i")
              , Basic (Unmarked " ")
              , Basic (Bold "b")
          ])
    ]
    , testGroup "everything" [
        testCase "unmarked" $
          parse everything "" "abc" @?= Right (Content [ Markdown [ Basic (Unmarked "abc") ] ])
      , testCase "latex" $
          parse everything "" "abc $x$ $$y$$" @?= Right (
            Content [
              Markdown [ Basic (Unmarked "abc ") ]
            , InlineMath "x"
            , Markdown [ Basic (Unmarked " ") ]
            , BlockMath "y"
            ]
          )
    ]
  ]
