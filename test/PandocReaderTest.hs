{-# LANGUAGE OverloadedStrings #-}

module PandocReaderTest (tests) where

import Automerge as A (BlockType (OrderedListItemType, UnorderedListItemType), Mark (..))
import AutomergeTestUtils as Automerge (codeTextSpan, emphasisTextSpan, heading1Span, heading4Span, linkTextSpan, orderedListItemSpan, paragraphSpan, strongTextSpan, textSpan, textSpanWithMarks, unorderedListItemSpan)
import PandocReader (toPandoc)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc.Builder as Pandoc (bulletList, code, doc, emph, fromList, header, link, orderedList, para, plain, str, strong, toList)
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