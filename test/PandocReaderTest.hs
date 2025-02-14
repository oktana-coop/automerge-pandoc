{-# LANGUAGE OverloadedStrings #-}

module PandocReaderTest (tests) where

import Automerge as A (Mark (..))
import AutomergeTestUtils as Automerge (emphasisTextSpan, heading1Span, paragraphSpan, strongTextSpan, textSpan, textSpanWithMarks)
import PandocReader (toPandoc)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc.Builder as Pandoc (doc, emph, fromList, header, para, str, strong, toList)
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