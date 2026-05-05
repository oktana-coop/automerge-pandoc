{-# LANGUAGE OverloadedStrings #-}

module PandocWriterTest (tests) where

import Automerge as A (BlockType (..), parseAutomergeSpansText)
import qualified Automerge as Automerge (Span)
import AutomergeTestUtils as Automerge (blockQuoteSpan, captionSpan, emphasisTextSpan, figureSpan, heading1Span, horizontalRuleSpan, imageSpan, noteContentSpan, noteRefSpan, orderedListItemSpan, paragraphSpan, strongTextSpan, textSpan, unorderedListItemSpan)
import PandocWriter (writeAutomerge)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc (def)
import Text.Pandoc.Builder as Pandoc (Pandoc, blockQuote, bulletList, doc, emph, emptyCaption, figure, header, horizontalRule, image, note, orderedList, para, plain, simpleCaption, str, strong)
import Text.Pandoc.Class (runIO)

tests :: IO TestTree
tests = do
  hspecTests <- testSpec "hspec" spec
  return $ testGroup "Writer" [hspecTests]

spec :: Spec
spec = do
  it "writes a document with a heading and paragraphs" $ do
    let input =
          Pandoc.doc $
            Pandoc.header 1 (Pandoc.str "A Heading 1")
              <> Pandoc.para (Pandoc.str "A paragraph")
              <> Pandoc.para (Pandoc.str "Another paragraph")

        expected =
          [ Automerge.heading1Span [],
            Automerge.textSpan "A Heading 1",
            Automerge.paragraphSpan [],
            Automerge.textSpan "A paragraph",
            Automerge.paragraphSpan [],
            Automerge.textSpan "Another paragraph"
          ]

    assertWritesSpans input expected

  describe "Marks" $ do
    it "writes interleaved marks as separate text spans" $ do
      let input =
            Pandoc.doc $
              Pandoc.para $
                Pandoc.str "Before "
                  <> Pandoc.strong (Pandoc.str "bold")
                  <> Pandoc.str " mid "
                  <> Pandoc.emph (Pandoc.str "emph")
                  <> Pandoc.str " after"

          expected =
            [ Automerge.paragraphSpan [],
              Automerge.textSpan "Before ",
              Automerge.strongTextSpan "bold",
              Automerge.textSpan " mid ",
              Automerge.emphasisTextSpan "emph",
              Automerge.textSpan " after"
            ]

      assertWritesSpans input expected

  describe "Lists" $ do
    it "writes an unordered list with text items" $ do
      let input =
            Pandoc.doc $
              Pandoc.bulletList
                [ Pandoc.plain (Pandoc.str "Item 1"),
                  Pandoc.plain (Pandoc.str "Item 2")
                ]

          expected =
            [ Automerge.unorderedListItemSpan [],
              Automerge.textSpan "Item 1",
              Automerge.unorderedListItemSpan [],
              Automerge.textSpan "Item 2"
            ]

      assertWritesSpans input expected

    it "writes a nested ordered list with the correct parent chain" $ do
      let input =
            Pandoc.doc $
              Pandoc.orderedList
                [ Pandoc.plain (Pandoc.str "Outer 1")
                    <> Pandoc.orderedList [Pandoc.plain (Pandoc.str "Inner 1.1")]
                ]

          expected =
            [ Automerge.orderedListItemSpan [],
              Automerge.textSpan "Outer 1",
              Automerge.orderedListItemSpan [A.OrderedListItemType],
              Automerge.textSpan "Inner 1.1"
            ]

      assertWritesSpans input expected

  describe "BlockQuote" $ do
    it "writes a blockquote with nested paragraphs" $ do
      let input =
            Pandoc.doc $
              Pandoc.blockQuote $
                Pandoc.para (Pandoc.str "First quoted paragraph")
                  <> Pandoc.para (Pandoc.str "Second quoted paragraph")

          expected =
            [ Automerge.blockQuoteSpan [],
              Automerge.paragraphSpan [A.BlockQuoteType],
              Automerge.textSpan "First quoted paragraph",
              Automerge.paragraphSpan [A.BlockQuoteType],
              Automerge.textSpan "Second quoted paragraph"
            ]

      assertWritesSpans input expected

  describe "HorizontalRule" $ do
    it "writes a horizontal rule between paragraphs" $ do
      let input =
            Pandoc.doc $
              Pandoc.para (Pandoc.str "Above")
                <> Pandoc.horizontalRule
                <> Pandoc.para (Pandoc.str "Below")

          expected =
            [ Automerge.paragraphSpan [],
              Automerge.textSpan "Above",
              Automerge.horizontalRuleSpan [],
              Automerge.paragraphSpan [],
              Automerge.textSpan "Below"
            ]

      assertWritesSpans input expected

  describe "Notes" $ do
    it "appends single-block note content after the main spans" $ do
      let input =
            Pandoc.doc $
              Pandoc.para $
                Pandoc.str "Before "
                  <> Pandoc.note (Pandoc.plain (Pandoc.str "The note text"))

          expected =
            [ Automerge.paragraphSpan [],
              Automerge.textSpan "Before ",
              Automerge.noteRefSpan [A.ParagraphType] "1",
              Automerge.noteContentSpan [] "1",
              Automerge.textSpan "The note text"
            ]

      assertWritesSpans input expected

    it "writes a note with multi-block content" $ do
      let input =
            Pandoc.doc $
              Pandoc.para $
                Pandoc.note (Pandoc.para (Pandoc.str "Para 1") <> Pandoc.para (Pandoc.str "Para 2"))

          expected =
            [ Automerge.paragraphSpan [],
              Automerge.noteRefSpan [A.ParagraphType] "1",
              Automerge.noteContentSpan [] "1",
              Automerge.paragraphSpan [A.NoteContentType],
              Automerge.textSpan "Para 1",
              Automerge.paragraphSpan [A.NoteContentType],
              Automerge.textSpan "Para 2"
            ]

      assertWritesSpans input expected

  describe "Images" $ do
    it "writes an image with title and alt text inside a paragraph" $ do
      let input =
            Pandoc.doc $
              Pandoc.para $
                Pandoc.str "Before "
                  <> Pandoc.image "image.png" "Image title" (Pandoc.str "alt")

          expected =
            [ Automerge.paragraphSpan [],
              Automerge.textSpan "Before ",
              Automerge.imageSpan [A.ParagraphType] "image.png" (Just "Image title") (Just "alt")
            ]

      assertWritesSpans input expected

  describe "Figures" $ do
    it "writes a figure with no caption" $ do
      let input =
            Pandoc.doc $
              Pandoc.figure
                Pandoc.emptyCaption
                (Pandoc.plain $ Pandoc.image "image.png" "" (Pandoc.str "alt"))

          expected =
            [ Automerge.figureSpan [],
              Automerge.imageSpan [A.FigureType] "image.png" Nothing (Just "alt")
            ]

      assertWritesSpans input expected

    it "emits figure blocks before the caption" $ do
      let input =
            Pandoc.doc $
              Pandoc.figure
                (Pandoc.simpleCaption (Pandoc.para (Pandoc.str "Caption text")))
                (Pandoc.plain $ Pandoc.image "image.png" "" (Pandoc.str "alt"))

          expected =
            [ Automerge.figureSpan [],
              Automerge.imageSpan [A.FigureType] "image.png" Nothing (Just "alt"),
              Automerge.captionSpan [A.FigureType],
              Automerge.paragraphSpan [A.FigureType, A.CaptionType],
              Automerge.textSpan "Caption text"
            ]

      assertWritesSpans input expected

assertWritesSpans :: Pandoc.Pandoc -> [Automerge.Span] -> IO ()
assertWritesSpans input expected = do
  result <- runIO $ writeAutomerge def input
  case result of
    Left err -> expectationFailure ("writeAutomerge failed: " <> show err)
    Right jsonText -> case parseAutomergeSpansText jsonText of
      Left parseErr -> expectationFailure ("parseAutomergeSpansText failed: " <> parseErr)
      Right actual -> actual `shouldBe` expected
