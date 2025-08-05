{-# LANGUAGE OverloadedStrings #-}

module PandocReaderTest (tests) where

import Automerge as A (BlockType (..), Mark (..))
import AutomergeTestUtils as Automerge (blockQuoteSpan, codeTextSpan, emphasisTextSpan, heading1Span, heading2Span, heading4Span, linkTextSpan, noteContentSpan, noteRefSpan, orderedListItemSpan, paragraphSpan, strongTextSpan, textSpan, textSpanWithMarks, unorderedListItemSpan)
import PandocReader (toPandoc)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc.Builder as Pandoc (blockQuote, bulletList, code, doc, emph, fromList, header, link, note, orderedList, para, plain, str, strong, toList)
import Text.Pandoc.Class (runIO)

tests :: IO TestTree
tests = do
  hspecTests <- testSpec "hspec" spec
  return $ testGroup "Reader" [hspecTests]

spec :: Spec
spec = do
  it "handles a list of empty automerge spans" $ do
    let input = []
        expected = fromList []

    result <- runIO $ toPandoc input
    case result of
      Left err -> expectationFailure ("toPandoc failed: " <> show err)
      Right actual -> actual `shouldBe` Pandoc.doc expected

  it "handles a simple document with headings and plain text paragraphs" $ do
    let input =
          [ Automerge.heading1Span [],
            Automerge.textSpan "A Heading 1",
            Automerge.paragraphSpan [],
            Automerge.textSpan "A paragraph",
            Automerge.paragraphSpan [],
            Automerge.textSpan "Another paragraph"
          ]

        expected =
          fromList $
            concat
              [ toList $ Pandoc.header 1 $ Pandoc.str "A Heading 1",
                toList $ Pandoc.para $ Pandoc.str "A paragraph",
                toList $ Pandoc.para $ Pandoc.str "Another paragraph"
              ]

    result <- runIO $ toPandoc input
    case result of
      Left err -> expectationFailure ("toPandoc failed: " <> show err)
      Right actual -> actual `shouldBe` Pandoc.doc expected

  describe "Marks" $ do
    it "handles strong text" $ do
      let input =
            [ Automerge.paragraphSpan [],
              Automerge.strongTextSpan "Strong text"
            ]

          expected =
            fromList $
              concat
                [ toList $ Pandoc.para $ Pandoc.strong $ Pandoc.str "Strong text"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles emphasized text" $ do
      let input =
            [ Automerge.paragraphSpan [],
              Automerge.emphasisTextSpan "Emphasized text"
            ]

          expected =
            fromList $
              concat
                [ toList $ Pandoc.para $ Pandoc.emph $ Pandoc.str "Emphasized text"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles the combination of strong and emphasized text" $ do
      let input =
            [ Automerge.paragraphSpan [],
              Automerge.textSpanWithMarks "Both strong and emphasized text" [A.Strong, A.Emphasis]
            ]

          expected =
            fromList $
              concat
                -- TODO: There is no order in automerge marks but here the marks are inverted and it should be
                -- understood and investigated why this happens.
                [ toList $ Pandoc.para $ Pandoc.emph $ Pandoc.strong $ Pandoc.str "Both strong and emphasized text"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles links" $ do
      let input =
            [ Automerge.paragraphSpan [],
              Automerge.linkTextSpan "Automerge" "https://automerge.org/" "Automerge"
            ]

          expected =
            fromList $
              concat
                -- TODO: There is no order in automerge marks but here the marks are inverted and it should be
                -- understood and investigated why this happens.
                [ toList $ Pandoc.para $ Pandoc.link "https://automerge.org/" "Automerge" $ Pandoc.str "Automerge"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles inline code" $ do
      let input =
            [ Automerge.paragraphSpan [],
              Automerge.codeTextSpan "func1"
            ]

          expected =
            fromList $
              concat
                [ toList $ Pandoc.para $ Pandoc.code "func1"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles text that is partially covered with marks" $ do
      let input =
            [ Automerge.paragraphSpan [],
              Automerge.textSpan "Some plain text followed by ",
              Automerge.strongTextSpan "strong text",
              Automerge.textSpan " and a link: ",
              Automerge.linkTextSpan "Automerge" "https://automerge.org/" "Automerge"
            ]

          expected =
            fromList $
              concat
                [ toList $
                    Pandoc.para $
                      fromList $
                        concat
                          [ toList $ Pandoc.str "Some plain text followed by ",
                            toList $ Pandoc.strong $ Pandoc.str "strong text",
                            toList $ Pandoc.str " and a link: ",
                            toList $ Pandoc.link "https://automerge.org/" "Automerge" $ Pandoc.str "Automerge"
                          ]
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

  describe "Lists" $ do
    it "handles unordered lists with plain text children" $ do
      let input =
            [ Automerge.heading1Span [],
              Automerge.textSpan "A Heading 1",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph above the list",
              Automerge.unorderedListItemSpan [],
              Automerge.textSpan "List item 1",
              Automerge.unorderedListItemSpan [],
              Automerge.textSpan "List item 2",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph below the list"
            ]

          expected =
            fromList $
              concat
                [ toList $ Pandoc.header 1 $ Pandoc.str "A Heading 1",
                  toList $ Pandoc.para $ Pandoc.str "Paragraph above the list",
                  -- using `Plain` pandoc blocks for list item text spans
                  toList $ Pandoc.bulletList [Pandoc.plain $ Pandoc.str "List item 1", Pandoc.plain $ Pandoc.str "List item 2"],
                  toList $ Pandoc.para $ Pandoc.str "Paragraph below the list"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles ordered lists with plain text children" $ do
      let input =
            [ Automerge.heading1Span [],
              Automerge.textSpan "A Heading 1",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph above the list",
              Automerge.orderedListItemSpan [],
              Automerge.textSpan "List item 1",
              Automerge.orderedListItemSpan [],
              Automerge.textSpan "List item 2",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph below the list"
            ]

          expected =
            fromList $
              concat
                [ toList $ Pandoc.header 1 $ Pandoc.str "A Heading 1",
                  toList $ Pandoc.para $ Pandoc.str "Paragraph above the list",
                  -- using `Plain` pandoc blocks for list item text spans
                  toList $ Pandoc.orderedList [Pandoc.plain $ Pandoc.str "List item 1", Pandoc.plain $ Pandoc.str "List item 2"],
                  toList $ Pandoc.para $ Pandoc.str "Paragraph below the list"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "returns list item text that includes marks in a single Pandoc `Plain` block" $ do
      let input =
            [ Automerge.heading1Span [],
              Automerge.textSpan "A Heading 1",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph above the list",
              Automerge.unorderedListItemSpan [],
              Automerge.textSpan "List item 1 with some ",
              Automerge.strongTextSpan "strong text",
              Automerge.unorderedListItemSpan [],
              Automerge.textSpan "List item 2 with a ",
              Automerge.linkTextSpan "link" "https://automerge.org/" "Automerge",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph below the list"
            ]

          expected =
            fromList $
              concat
                [ toList $ Pandoc.header 1 $ Pandoc.str "A Heading 1",
                  toList $ Pandoc.para $ Pandoc.str "Paragraph above the list",
                  toList $
                    Pandoc.bulletList
                      [ Pandoc.plain (Pandoc.str "List item 1 with some " <> (Pandoc.strong $ Pandoc.str "strong text")),
                        Pandoc.plain (Pandoc.str "List item 2 with a " <> (Pandoc.link "https://automerge.org/" "Automerge" $ str "link"))
                      ],
                  toList $ Pandoc.para $ Pandoc.str "Paragraph below the list"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles nested lists with plain text children" $ do
      let input =
            [ Automerge.heading1Span [],
              Automerge.textSpan "A Heading 1",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph above the list",
              Automerge.unorderedListItemSpan [],
              Automerge.textSpan "List item 1",
              Automerge.unorderedListItemSpan [A.UnorderedListItemType],
              Automerge.textSpan "List item 1.1",
              Automerge.unorderedListItemSpan [A.UnorderedListItemType],
              Automerge.textSpan "List item 1.2",
              Automerge.unorderedListItemSpan [],
              Automerge.textSpan "List item 2",
              Automerge.unorderedListItemSpan [A.UnorderedListItemType],
              Automerge.textSpan "List item 2.1",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph below the list"
            ]

          expected =
            fromList $
              concat
                [ toList $ Pandoc.header 1 $ Pandoc.str "A Heading 1",
                  toList $ Pandoc.para $ Pandoc.str "Paragraph above the list",
                  toList $
                    Pandoc.bulletList
                      [ ( (Pandoc.plain $ str "List item 1")
                            <> ( Pandoc.bulletList
                                   [ Pandoc.plain $ Pandoc.str "List item 1.1",
                                     Pandoc.plain $ Pandoc.str "List item 1.2"
                                   ]
                               )
                        ),
                        ( (Pandoc.plain $ Pandoc.str "List item 2")
                            <> ( Pandoc.bulletList
                                   [Pandoc.plain $ Pandoc.str "List item 2.1"]
                               )
                        )
                      ],
                  toList $ Pandoc.para $ Pandoc.str "Paragraph below the list"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles nested lists with child blocks and two levels of nesting" $ do
      let input =
            [ Automerge.heading1Span [],
              Automerge.textSpan "A Heading 1",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph above the list",
              Automerge.orderedListItemSpan [],
              Automerge.textSpan "List item 1",
              Automerge.heading4Span [A.OrderedListItemType],
              Automerge.textSpan "Heading 4 inside a list item",
              Automerge.orderedListItemSpan [A.OrderedListItemType],
              Automerge.textSpan "List item 1.1",
              Automerge.orderedListItemSpan [A.OrderedListItemType, A.OrderedListItemType],
              Automerge.textSpan "List item 1.1.1",
              Automerge.paragraphSpan [A.OrderedListItemType, A.OrderedListItemType, A.OrderedListItemType],
              Automerge.textSpan "Paragraph nested 3 levels deep",
              Automerge.orderedListItemSpan [A.OrderedListItemType],
              Automerge.textSpan "List item 1.2",
              Automerge.orderedListItemSpan [],
              Automerge.textSpan "List item 2",
              Automerge.orderedListItemSpan [A.OrderedListItemType],
              Automerge.textSpan "List item 2.1",
              Automerge.paragraphSpan [A.OrderedListItemType, A.OrderedListItemType],
              Automerge.textSpan "Paragraph nested 2 levels deep",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph below the list"
            ]

          expected =
            fromList $
              concat
                [ toList $ Pandoc.header 1 $ Pandoc.str "A Heading 1",
                  toList $ Pandoc.para $ Pandoc.str "Paragraph above the list",
                  toList $
                    Pandoc.orderedList
                      [ ( (plain $ str "List item 1")
                            <> (Pandoc.header 4 $ Pandoc.str "Heading 4 inside a list item")
                            <> ( Pandoc.orderedList
                                   [ ( (Pandoc.plain $ Pandoc.str "List item 1.1")
                                         <> Pandoc.orderedList
                                           [ ( (Pandoc.plain $ Pandoc.str "List item 1.1.1")
                                                 <> (Pandoc.para $ Pandoc.str "Paragraph nested 3 levels deep")
                                             )
                                           ]
                                     ),
                                     Pandoc.plain $ Pandoc.str "List item 1.2"
                                   ]
                               )
                        ),
                        ( (plain $ str "List item 2")
                            <> ( Pandoc.orderedList
                                   [ ( (Pandoc.plain $ Pandoc.str "List item 2.1")
                                         <> (Pandoc.para $ Pandoc.str "Paragraph nested 2 levels deep")
                                     )
                                   ]
                               )
                        )
                      ],
                  toList $ Pandoc.para $ Pandoc.str "Paragraph below the list"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

  describe "BlockQuote" $ do
    it "handles a blockquote with nested paragraphs" $ do
      let input =
            [ Automerge.heading1Span [],
              Automerge.textSpan "A Heading 1",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph above the blockquote",
              Automerge.blockQuoteSpan [],
              Automerge.paragraphSpan [A.BlockQuoteType],
              Automerge.textSpan "Ticking away the moments that make up a dull day",
              Automerge.paragraphSpan [A.BlockQuoteType],
              Automerge.textSpan "You fritter and waste the hours in an offhand way",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph below the blockquote"
            ]

          expected =
            fromList $
              concat
                [ toList $ Pandoc.header 1 $ Pandoc.str "A Heading 1",
                  toList $ Pandoc.para $ Pandoc.str "Paragraph above the blockquote",
                  toList $
                    Pandoc.blockQuote $
                      fromList $
                        concat $
                          [ toList $ (Pandoc.para $ str "Ticking away the moments that make up a dull day"),
                            toList $ (Pandoc.para $ str "You fritter and waste the hours in an offhand way")
                          ],
                  toList $ Pandoc.para $ Pandoc.str "Paragraph below the blockquote"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "wraps a text span just after the blockquote in a Plain block" $ do
      let input =
            [ Automerge.heading1Span [],
              Automerge.textSpan "A Heading 1",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph above the blockquote",
              Automerge.blockQuoteSpan [],
              Automerge.textSpan "Ticking away the moments that make up a dull day",
              Automerge.paragraphSpan [A.BlockQuoteType],
              Automerge.textSpan "You fritter and waste the hours in an offhand way",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph below the blockquote"
            ]

          expected =
            fromList $
              concat
                [ toList $ Pandoc.header 1 $ Pandoc.str "A Heading 1",
                  toList $ Pandoc.para $ Pandoc.str "Paragraph above the blockquote",
                  toList $
                    Pandoc.blockQuote $
                      fromList $
                        concat $
                          [ toList $ (Pandoc.plain $ str "Ticking away the moments that make up a dull day"),
                            toList $ (Pandoc.para $ str "You fritter and waste the hours in an offhand way")
                          ],
                  toList $ Pandoc.para $ Pandoc.str "Paragraph below the blockquote"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles a heading and marks inside the blockquote" $ do
      let input =
            [ Automerge.heading1Span [],
              Automerge.textSpan "A Heading 1",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph above the blockquote",
              Automerge.blockQuoteSpan [],
              Automerge.heading2Span [A.BlockQuoteType],
              Automerge.textSpan "Time",
              Automerge.paragraphSpan [A.BlockQuoteType],
              Automerge.strongTextSpan "Ticking away",
              Automerge.textSpan " the moments that make up a dull day",
              Automerge.paragraphSpan [A.BlockQuoteType],
              Automerge.textSpan "You fritter and waste the hours in an offhand way",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Paragraph below the blockquote"
            ]

          expected =
            fromList $
              concat
                [ toList $ Pandoc.header 1 $ Pandoc.str "A Heading 1",
                  toList $ Pandoc.para $ Pandoc.str "Paragraph above the blockquote",
                  toList $
                    Pandoc.blockQuote $
                      fromList $
                        concat $
                          [ toList $ Pandoc.header 2 $ Pandoc.str "Time",
                            toList $
                              ( Pandoc.para $
                                  fromList $
                                    concat
                                      [ toList $ Pandoc.strong $ Pandoc.str "Ticking away",
                                        toList $ Pandoc.str " the moments that make up a dull day"
                                      ]
                              ),
                            toList $ (Pandoc.para $ str "You fritter and waste the hours in an offhand way")
                          ],
                  toList $ Pandoc.para $ Pandoc.str "Paragraph below the blockquote"
                ]

      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected
  describe "Notes" $ do
    it "converts a note reference and its content into a Pandoc note" $ do
      let noteId = "1"
          input =
            [ Automerge.paragraphSpan [],
              Automerge.textSpan "A paragraph ",
              Automerge.noteRefSpan [A.ParagraphType] noteId,
              Automerge.paragraphSpan [],
              Automerge.textSpan "Another paragraph",
              Automerge.noteContentSpan [] noteId,
              Automerge.textSpan "This is a note"
            ]

          expected =
            fromList $
              concat
                [ toList $
                    Pandoc.para $
                      fromList $
                        concat $
                          [ toList $ Pandoc.str "A paragraph ",
                            toList $
                              Pandoc.note $
                                fromList $
                                  concat $
                                    [ toList $
                                        (Pandoc.plain $ Pandoc.str "This is a note")
                                    ]
                          ],
                  toList $
                    Pandoc.para $
                      Pandoc.str "Another paragraph"
                ]
      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles consecutive notes at the end of a paragraph" $ do
      let input =
            [ Automerge.paragraphSpan [],
              Automerge.textSpan "A paragraph ",
              Automerge.noteRefSpan [A.ParagraphType] "1",
              Automerge.noteRefSpan [A.ParagraphType] "2",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Another paragraph",
              Automerge.noteContentSpan [] "1",
              Automerge.textSpan "Note 1 content",
              Automerge.noteContentSpan [] "2",
              Automerge.textSpan "Note 2 content"
            ]

          expected =
            fromList $
              concat
                [ toList $
                    Pandoc.para $
                      fromList $
                        concat $
                          [ toList $ Pandoc.str "A paragraph ",
                            toList $
                              Pandoc.note $
                                fromList $
                                  concat $
                                    [ toList $
                                        (Pandoc.plain $ Pandoc.str "Note 1 content")
                                    ],
                            toList $
                              Pandoc.note $
                                fromList $
                                  concat $
                                    [ toList $
                                        (Pandoc.plain $ Pandoc.str "Note 2 content")
                                    ]
                          ],
                  toList $
                    Pandoc.para $
                      Pandoc.str "Another paragraph"
                ]
      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles notes between other text" $ do
      let input =
            [ Automerge.paragraphSpan [],
              Automerge.textSpan "A paragraph ",
              Automerge.noteRefSpan [A.ParagraphType] "1",
              Automerge.textSpan " ",
              Automerge.noteRefSpan [A.ParagraphType] "2",
              Automerge.textSpan " continuing after the note.",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Another paragraph",
              Automerge.noteContentSpan [] "1",
              Automerge.textSpan "Note 1 content",
              Automerge.noteContentSpan [] "2",
              Automerge.textSpan "Note 2 content"
            ]

          expected =
            fromList $
              concat
                [ toList $
                    Pandoc.para $
                      fromList $
                        concat $
                          [ toList $ Pandoc.str "A paragraph ",
                            toList $
                              Pandoc.note $
                                fromList $
                                  concat $
                                    [ toList $
                                        (Pandoc.plain $ Pandoc.str "Note 1 content")
                                    ],
                            toList $ Pandoc.str " ",
                            toList $
                              Pandoc.note $
                                fromList $
                                  concat $
                                    [ toList $
                                        (Pandoc.plain $ Pandoc.str "Note 2 content")
                                    ],
                            toList $ Pandoc.str " continuing after the note."
                          ],
                  toList $
                    Pandoc.para $
                      Pandoc.str "Another paragraph"
                ]
      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "handles multi-block note content" $ do
      let input =
            [ Automerge.paragraphSpan [],
              Automerge.textSpan "A paragraph ",
              Automerge.noteRefSpan [A.ParagraphType] "1",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Another paragraph",
              Automerge.noteContentSpan [] "1",
              Automerge.paragraphSpan [A.NoteContentType],
              Automerge.textSpan "Note content paragraph 1",
              Automerge.paragraphSpan [A.NoteContentType],
              Automerge.textSpan "Note content paragraph 2"
            ]

          expected =
            fromList $
              concat
                [ toList $
                    Pandoc.para $
                      fromList $
                        concat $
                          [ toList $ Pandoc.str "A paragraph ",
                            toList $
                              Pandoc.note $
                                fromList $
                                  concat $
                                    [ toList $
                                        (Pandoc.para $ Pandoc.str "Note content paragraph 1"),
                                      toList $
                                        (Pandoc.para $ Pandoc.str "Note content paragraph 2")
                                    ]
                          ],
                  toList $
                    Pandoc.para $
                      Pandoc.str "Another paragraph"
                ]
      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "ignores a note reference without content" $ do
      let input =
            [ Automerge.paragraphSpan [],
              Automerge.textSpan "A paragraph ",
              Automerge.noteRefSpan [A.ParagraphType] "1",
              Automerge.noteRefSpan [A.ParagraphType] "2",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Another paragraph",
              Automerge.noteContentSpan [] "1",
              Automerge.textSpan "Note 1 content"
            ]

          expected =
            fromList $
              concat
                [ toList $
                    Pandoc.para $
                      fromList $
                        concat $
                          [ toList $ Pandoc.str "A paragraph ",
                            toList $
                              Pandoc.note $
                                fromList $
                                  concat $
                                    [ toList $
                                        (Pandoc.plain $ Pandoc.str "Note 1 content")
                                    ]
                          ],
                  toList $
                    Pandoc.para $
                      Pandoc.str "Another paragraph"
                ]
      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected

    it "ingores note content without a corresponding reference" $ do
      let input =
            [ Automerge.paragraphSpan [],
              Automerge.textSpan "A paragraph ",
              Automerge.noteRefSpan [A.ParagraphType] "1",
              Automerge.paragraphSpan [],
              Automerge.textSpan "Another paragraph",
              Automerge.noteContentSpan [] "1",
              Automerge.textSpan "Note 1 content",
              Automerge.noteContentSpan [] "2",
              Automerge.textSpan "Note 2 content"
            ]

          expected =
            fromList $
              concat
                [ toList $
                    Pandoc.para $
                      fromList $
                        concat $
                          [ toList $ Pandoc.str "A paragraph ",
                            toList $
                              Pandoc.note $
                                fromList $
                                  concat $
                                    [ toList $
                                        (Pandoc.plain $ Pandoc.str "Note 1 content")
                                    ]
                          ],
                  toList $
                    Pandoc.para $
                      Pandoc.str "Another paragraph"
                ]
      result <- runIO $ toPandoc input
      case result of
        Left err -> expectationFailure ("toPandoc failed: " <> show err)
        Right actual -> actual `shouldBe` Pandoc.doc expected