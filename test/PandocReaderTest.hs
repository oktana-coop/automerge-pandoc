{-# LANGUAGE OverloadedStrings #-}

module PandocReaderTest (tests) where

import PandocReader (toPandoc)
import Test.Hspec (Spec, expectationFailure, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc.Builder as Pandoc (doc, fromList)
import Text.Pandoc.Class (runIO)

tests :: IO TestTree
tests = do
  hspecTests <- testSpec "hspec" spec
  return $ testGroup "Reader" [hspecTests]

spec :: Spec
spec = do
  it "handles a list of empty automerge spans" $ do
    result <- runIO $ toPandoc []
    case result of
      Left err -> expectationFailure ("toPandoc failed: " <> show err)
      Right pandoc -> pandoc `shouldBe` Pandoc.doc (fromList [])
