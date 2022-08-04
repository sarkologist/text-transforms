{-# LANGUAGE OverloadedStrings #-}
module MarkdownParseTest where

import Test.Tasty
import Test.Tasty.HUnit

import Types
import MarkdownParse

import Text.Parsec (parse)

test_individual :: TestTree
test_individual =
  testGroup "individual"
  [
    testCase "unmarked" $
      parse unmarked "" "abc" @?= Right (Unmarked "abc")
  , testCase "newline" $
      parse newlineMarkdown "" "\n" @?= Right (Newline "\n")
  , testCase "unmarked only" $
      parse unmarked "" "abc*i*" @?= Right (Unmarked "abc")
  , testCase "italic" $
      parse italic "" "*abc*" @?= Right (Italic "abc")
  , testCase "bold" $
      parse bold "" "**abc**" @?= Right (Bold "abc")
  , testGroup "latex" [
        testCase "inline" $
          parse inlineMath "" "$abc$" @?= Right (InlineMath "abc")
      ]
  , testCase "header simple" $
      parse (header 1) "" "# abc\n" @?= Right (Header 1 [ Unmarked "abc" ])
  , testCase "header 2" $
      parse (header 2) "" "## abc\n" @?= Right (Header 2 [ Unmarked "abc" ])
  , testCase "header complex" $
      parse (header 1) "" "# abc **bold**\n" @?= Right (Header 1 [ Unmarked "abc ", Bold "bold" ])
  , testGroup "bullets" [
      testCase "one item" $
        parse (bullets 0) "" "- a\n" @?= Right (Bullets [BulletLeaf [ Unmarked "a" ]])
    , testCase "two items" $
        parse (bullets 0) "" "- a\n- b\n" @?= Right (Bullets [
          BulletLeaf [ Unmarked "a" ],
          BulletLeaf [ Unmarked "b" ]
        ])
    , testCase "not followed by final newline" $
        parse (bullets 0) "" "- a" @?= Right (Bullets [BulletLeaf [ Unmarked "a" ]])
    , testCase "nested" $
        parse (bullets 0) ""
          "- a\n\
          \  - b\n\
          \  - c\n" @?= Right (Bullets [
          BulletLeaf [ Unmarked "a" ]
        , BulletRecurse $ Bullets [
            BulletLeaf [ Unmarked "b" ]
          , BulletLeaf [ Unmarked "c" ]
          ]
        ])
    , testCase "nested back up level" $
        parse (bullets 0) ""
          "- a\n\
          \  - b\n\
          \  - c\n\
          \- d\n" @?= Right (Bullets [
          BulletLeaf [ Unmarked "a" ]
        , BulletRecurse $ Bullets [
            BulletLeaf [ Unmarked "b" ]
          , BulletLeaf [ Unmarked "c" ]
          ]
        , BulletLeaf [ Unmarked "d" ]
        ])
    ]
  ]


test_complex =
  testGroup "complex"
  [
      testGroup "unmarked" $
      [
        testCase "non-greedy" $
          parse (unmarked *> italic) "" "abc *i*" @?= Right ((Italic "i") )
      ]
    , testGroup "newline" $
      [
        testCase "in markdown" $
          parse markdown "" "abc\n123" @?= Right (Markdown $ [
            Basic $ Unmarked "abc"
          , Newline "\n"
          , Basic $ Unmarked "123"
          ] )
      ]
    , testGroup "markdown"
    [
      testCase "unmarked" $
        parse markdown "" "abc" @?= Right (Markdown [ Basic (Unmarked "abc") ])
    , testCase "basic" $
        parse markdown "" "abc *i* **b** $inline$" @?=
          Right (Markdown . fmap Basic $ [
                Unmarked "abc "
              , Italic "i"
              , Unmarked " "
              , Bold "b"
              , Unmarked " "
              , InlineMath "inline"
          ])
     , testGroup "bullet" [
          testCase "follows unmarked" $
            parse markdown "" "abc\n- bullet\n" @?= Right (Markdown [
                Basic (Unmarked "abc")
              , Newline "\n"
              , MarkdownBullets $ Bullets [ BulletLeaf [ Unmarked "bullet" ] ]
              ])
        ]
    ]
    , testGroup "everything" [
        testCase "unmarked" $
          parse everything "" "abc" @?= Right (Content [ Markdown [ Basic (Unmarked "abc") ] ])
      , testCase "latex" $
          parse everything "" "abc $x$ $$y$$" @?= Right (
            Content [
              Markdown . fmap Basic $ [
                  Unmarked "abc "
                , InlineMath "x"
                , Unmarked " "
                ]
            , BlockMath "y"
            ]
          )
    ]
  ]
