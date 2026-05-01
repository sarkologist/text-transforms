{-# LANGUAGE OverloadedStrings #-}
module MarkdownParseTest where

import Test.Tasty
import Test.Tasty.HUnit

import Types
import MarkdownParse

import Text.Parsec (char, eof, parse)
import qualified Data.Text as T

ul = Bullets UnorderedList
ol = Bullets OrderedList
leaf = BulletLeaf . fmap Basic

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
  , testGroup "blockquotes" [
      testCase "single line" $
        parse blockquote "" "> quoted\n" @?= Right [
          Basic . BasicInline . Unmarked $ "quoted"
        ]
    , testCase "multi line with inline formatting" $
        parse blockquote "" "> **Careful**\n> $x$ matters\n" @?= Right [
            Basic $ Bold [Unmarked "Careful"]
          , Newline "\n"
          , Basic . BasicInline . InlineMath $ "x"
          , Basic . BasicInline . Unmarked $ " matters"
        ]
    ]
  , testGroup "bullets" [
      testCase "one item" $
        parse (bullets 0) "" "- a\n" @?= Right (ul [leaf [ BasicInline . Unmarked $ "a" ]])
    , testCase "two items" $
        parse (bullets 0) "" "- a\n- b\n" @?= Right (ul [
          leaf [ BasicInline . Unmarked $ "a" ],
          leaf [ BasicInline . Unmarked $ "b" ]
        ])
    , testCase "not followed by final newline" $
        parse (bullets 0) "" "- a" @?= Right (ul [leaf [ BasicInline . Unmarked $ "a" ]])
    , testCase "not followed by empty line" $
        parse (bullets 0) "" "- a\n something" @?= Right (ul [leaf [ BasicInline . Unmarked $ "a" ]])
    , testCase "nested" $
        parse (bullets 0) ""
          "- a\n\
          \  - b\n\
          \  - c\n" @?= Right (ul [
          leaf [ BasicInline . Unmarked $ "a" ]
        , BulletRecurse $ ul [
            leaf [ BasicInline . Unmarked $ "b" ]
          , leaf [ BasicInline . Unmarked $ "c" ]
          ]
        ])
    , testCase "nested back up level" $
        parse (bullets 0) ""
          "- a\n\
          \  - b\n\
          \  - c\n\
          \- d\n" @?= Right (ul [
          leaf [ BasicInline . Unmarked $ "a" ]
        , BulletRecurse $ ul [
            leaf [ BasicInline . Unmarked $ "b" ]
          , leaf [ BasicInline . Unmarked $ "c" ]
          ]
        , leaf [ BasicInline . Unmarked $ "d" ]
        ])
    , testCase "nested 3 levels" $
        parse (bullets 0) ""
          "- a\n\
          \  - b\n\
          \    - c\n" @?= Right (ul [
          leaf [ BasicInline . Unmarked $ "a" ]
        , BulletRecurse $ ul [
            leaf [ BasicInline . Unmarked $ "b" ]
          , BulletRecurse $ ul [
              leaf [ BasicInline . Unmarked $ "c" ]
            ]
          ]
        ])
    , testCase "numbered" $
        parse (numbered 0) "" "1. a\n2. b\n" @?= Right (ol [
          leaf [ BasicInline . Unmarked $ "a" ],
          leaf [ BasicInline . Unmarked $ "b" ]
        ])
    , testCase "numbered multiline with block math" $
        parse (numbered 0) "" (
            "1. first line with $x$\n"
         <> "   $$y$$\n"
         <> "   after math\n"
         <> "\n"
         <> "2. second\n"
        ) @?= Right (ol [
          BulletLeaf [
              Basic . BasicInline . Unmarked $ "first line with "
            , Basic . BasicInline . InlineMath $ "x"
            , Newline "\n"
            , MarkdownBlockMath "y"
            , Basic . BasicInline . Unmarked $ "after math"
            ]
        , leaf [ BasicInline . Unmarked $ "second" ]
        ])
    , testCase "mixed nested numbering" $
        parse (bullets 0) ""
          "- a\n\
          \  1. b\n\
          \  2. c\n" @?= Right (ul [
          leaf [ BasicInline . Unmarked $ "a" ]
        , BulletRecurse $ ol [
            leaf [ BasicInline . Unmarked $ "b" ]
          , leaf [ BasicInline . Unmarked $ "c" ]
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
    , testGroup "blockquote" [
          testCase "starts document" $
            parse (markdown eof) "" "> [!warning]\n> Condition 1 alone is not sufficient!\n" @?= Right (Markdown [
              Blockquote [
                  Basic . BasicInline . Unmarked $ "[!warning]"
                , Newline "\n"
                , Basic . BasicInline . Unmarked $ "Condition 1 alone is not sufficient!"
                ]
              ])
        , testCase "admonition title with math and numbered lines" $
            parse (markdown eof) "" "> [!theorem] Classification on $\\mathbb{C}/L$\n> 1. $\\deg(D) = 0$.\n> 2. $\\sum_k n_k z_k \\in L$.\n" @?= Right (Markdown [
              Blockquote [
                  Basic . BasicInline . Unmarked $ "[!theorem] Classification on "
                , Basic . BasicInline . InlineMath $ "\\mathbb{C}/L"
                , Newline "\n"
                , MarkdownBullets $ ol [
                    leaf [
                        BasicInline . InlineMath $ "\\deg(D) = 0"
                      , BasicInline . Unmarked $ "."
                      ]
                  , leaf [
                        BasicInline . InlineMath $ "\\sum_k n_k z_k \\in L"
                      , BasicInline . Unmarked $ "."
                      ]
                  ]
                ]
              ])
        ]
     , testGroup "bullet" [
          testCase "follows unmarked" $
            parse (markdown eof) "" "abc\n- bullet\n" @?= Right (Markdown [
                Basic (BasicInline (Unmarked "abc"))
              , Newline "\n"
              , MarkdownBullets $ ul [ leaf [ BasicInline (Unmarked "bullet") ] ]
              ])
      , testCase "numbered list follows unmarked" $
          parse (markdown eof) "" "abc\n1. first\n2. second\n" @?= Right (Markdown [
              Basic (BasicInline (Unmarked "abc"))
            , Newline "\n"
            , MarkdownBullets $ ol [
                leaf [ BasicInline (Unmarked "first") ]
              , leaf [ BasicInline (Unmarked "second") ]
              ]
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
