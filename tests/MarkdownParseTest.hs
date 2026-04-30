{-# LANGUAGE OverloadedStrings #-}
module MarkdownParseTest where

import Test.Tasty
import Test.Tasty.HUnit

import Types
import MarkdownParse

import Text.Parsec (char, eof, parse)
import qualified Data.Text as T

test_individual :: TestTree
test_individual =
  testGroup "individual"
  [
    testCase "unmarked" $
      parse (unmarked eof) "" ("abc" :: T.Text) @?= Right (Unmarked $ "abc")
  , testCase "newline" $
      parse newlineMarkdown "" "\n" @?= Right (Newline "\n")
  , testCase "unmarked only" $
      parse (unmarked (char '*')) "" ("abc*i*" :: T.Text) @?= Right (Unmarked $ "abc")
  , testCase "italic" $
      parse italic "" "*abc*" @?= Right (Italic [Unmarked "abc"])
  , testCase "math inside italic" $
      parse italic "" "*a$b$c*" @?= Right (Italic  [Unmarked "a",InlineMath "b",Unmarked "c"])
  , testCase "bold" $
      parse bold "" "**abc**" @?= Right (Bold [Unmarked "abc"])
  , testCase "math inside bold" $
      parse bold "" "**a$b$c**" @?= Right (Bold  [Unmarked "a",InlineMath "b",Unmarked "c"])
  , testGroup "latex" [
        testCase "inline" $
          parse inlineMath "" "$abc$" @?= Right (InlineMath "abc")
      ]
  , testCase "header simple" $
      parse (header 1) "" "# abc\n" @?= Right (Header 1 [ BasicInline . Unmarked $ "abc" ])
  , testCase "header 2" $
      parse (header 2) "" "## abc\n" @?= Right (Header 2 [ BasicInline . Unmarked $ "abc" ])
  , testCase "header complex" $
      parse (header 1) "" "# abc **bold**\n" @?= Right (Header 1 [
        BasicInline $ Unmarked "abc ",
        Bold [Unmarked "bold"]
      ])
  , testGroup "bullets" [
      testCase "one item" $
        parse (bullets 0) "" "- a\n" @?= Right (Bullets [BulletLeaf [ BasicInline . Unmarked $ "a" ]])
    , testCase "two items" $
        parse (bullets 0) "" "- a\n- b\n" @?= Right (Bullets [
          BulletLeaf [ BasicInline . Unmarked $ "a" ],
          BulletLeaf [ BasicInline . Unmarked $ "b" ]
        ])
    , testCase "not followed by final newline" $
        parse (bullets 0) "" "- a" @?= Right (Bullets [BulletLeaf [ BasicInline . Unmarked $ "a" ]])
    , testCase "not followed by empty line" $
        parse (bullets 0) "" "- a\n something" @?= Right (Bullets [BulletLeaf [ BasicInline . Unmarked $ "a" ]])
    , testCase "nested" $
        parse (bullets 0) ""
          "- a\n\
          \  - b\n\
          \  - c\n" @?= Right (Bullets [
          BulletLeaf [ BasicInline . Unmarked $ "a" ]
        , BulletRecurse $ Bullets [
            BulletLeaf [ BasicInline . Unmarked $ "b" ]
          , BulletLeaf [ BasicInline . Unmarked $ "c" ]
          ]
        ])
    , testCase "nested back up level" $
        parse (bullets 0) ""
          "- a\n\
          \  - b\n\
          \  - c\n\
          \- d\n" @?= Right (Bullets [
          BulletLeaf [ BasicInline . Unmarked $ "a" ]
        , BulletRecurse $ Bullets [
            BulletLeaf [ BasicInline . Unmarked $ "b" ]
          , BulletLeaf [ BasicInline . Unmarked $ "c" ]
          ]
        , BulletLeaf [ BasicInline . Unmarked $ "d" ]
        ])
    , testCase "nested 3 levels" $
        parse (bullets 0) ""
          "- a\n\
          \  - b\n\
          \    - c\n" @?= Right (Bullets [
          BulletLeaf [ BasicInline . Unmarked $ "a" ]
        , BulletRecurse $ Bullets [
            BulletLeaf [ BasicInline . Unmarked $ "b" ]
          , BulletRecurse $ Bullets [
              BulletLeaf [ BasicInline . Unmarked $ "c" ]
            ]
          ]
        ])
    ]
  , testGroup "tables" [
      testCase "basic" $
        parse table "" "| A | B |\n| --- | --- |\n| 1 | 2 |\n" @?= Right (Table [
            [ BasicInline . Unmarked $ "A" ]
          , [ BasicInline . Unmarked $ "B" ]
          ] [
            [
              [ BasicInline . Unmarked $ "1" ]
            , [ BasicInline . Unmarked $ "2" ]
            ]
          ])
    , testCase "inline formatting" $
        parse table "" "| A | B |\n| --- | --- |\n| *i* | **b** |\n" @?= Right (Table [
            [ BasicInline . Unmarked $ "A" ]
          , [ BasicInline . Unmarked $ "B" ]
          ] [
            [
              [ Italic [Unmarked "i"] ]
            , [ Bold [Unmarked "b"] ]
            ]
          ])
    ]
  ]


test_complex =
  testGroup "complex"
  [
      testGroup "unmarked" $
      [
        testCase "non-greedy" $
          parse (unmarked (char '*') *> italic) "" "abc *i*" @?= Right ((Italic [Unmarked "i"]) )
      ]
    , testGroup "newline" $
      [
        testCase "in markdown" $
          parse (markdown eof) "" "abc\n123" @?= Right (Markdown $ [
            Basic . BasicInline . Unmarked $ "abc"
          , Newline "\n"
          , Basic . BasicInline . Unmarked $ "123"
          ] )
      ]
    , testGroup "markdown"
    [
      testCase "unmarked" $
        parse (markdown eof) "" "abc" @?= Right (Markdown [ Basic (BasicInline (Unmarked "abc")) ])
    , testCase "basic" $
        parse (markdown eof) "" "abc *i* **b** $inline$" @?=
          Right (Markdown . fmap Basic $ [
                BasicInline . Unmarked $ "abc "
              , Italic [Unmarked "i"]
              , BasicInline . Unmarked $ " "
              , Bold [Unmarked "b"]
              , BasicInline . Unmarked $ " "
              , BasicInline . InlineMath $ "inline"
          ])
     , testGroup "bullet" [
          testCase "follows unmarked" $
            parse (markdown eof) "" "abc\n- bullet\n" @?= Right (Markdown [
                Basic (BasicInline (Unmarked "abc"))
              , Newline "\n"
              , MarkdownBullets $ Bullets [ BulletLeaf [ BasicInline (Unmarked "bullet") ] ]
              ])
        ]
    , testGroup "table" [
          testCase "follows unmarked" $
            parse (markdown eof) "" "abc\n| A | B |\n| --- | --- |\n| 1 | 2 |\n" @?= Right (Markdown [
                Basic (BasicInline (Unmarked "abc"))
              , Newline "\n"
              , MarkdownTable $ Table [
                  [ BasicInline . Unmarked $ "A" ]
                , [ BasicInline . Unmarked $ "B" ]
                ] [
                  [
                    [ BasicInline . Unmarked $ "1" ]
                  , [ BasicInline . Unmarked $ "2" ]
                  ]
                ]
              ])
        ]
    ]
    , testGroup "everything" [
        testCase "unmarked" $
          parse everything "" "abc" @?= Right (Content [ Markdown [ Basic (BasicInline (Unmarked "abc")) ] ])
      , testCase "latex" $
          parse everything "" "abc $x$ $$y$$" @?= Right (
            Content [
              Markdown . fmap Basic $ [
                  BasicInline . Unmarked $ "abc "
                , BasicInline . InlineMath $ "x"
                , BasicInline . Unmarked $ " "
                ]
            , BlockMath "y"
            ]
          )
    ]
  ]
