{-# LANGUAGE OverloadedStrings #-}

module Automerge where

import Data.Aeson
  ( FromJSON (parseJSON),
    withObject,
    withText,
    (.:),
  )
import Data.Aeson.Types (Parser)
import qualified Data.Text as T

data BlockType
  = Paragraph
  | Heading
  | CodeBlock
  | BlockQuote
  | OrderedListItem
  | UnorderedListItem
  | ImageBlock
  deriving (Show)

instance FromJSON BlockType where
  parseJSON = withText "BlockType" $ \t ->
    case t of
      "paragraph" -> pure Paragraph
      "heading" -> pure Heading
      "code-block" -> pure CodeBlock
      "blockquote" -> pure BlockQuote
      "ordered-list-item" -> pure OrderedListItem
      "unordered-list-item" -> pure UnorderedListItem
      "image" -> pure ImageBlock
      _ -> fail "Invalid value for BlockType"

data Mark
  = Strong
  | Emphasis
  | Link
  deriving (Show)

instance FromJSON Mark where
  parseJSON = withText "Mark" $ \t ->
    case t of
      "strong" -> pure Strong
      "em" -> pure Emphasis
      "link" -> pure Link
      _ -> fail "Invalid value for Mark"

data AutomergeSpan
  = BlockSpan {blockType :: BlockType}
  | TextSpan {value :: T.Text, marks :: [Mark]}
  deriving (Show)

instance FromJSON AutomergeSpan where
  parseJSON = withObject "AutomergeSpan" $ \v -> do
    elementType <- (v .: "type" :: Parser String)
    case elementType of
      "block" -> BlockSpan <$> (v .: "value" >>= (.: "type"))
      "text" -> TextSpan <$> v .: "value" <*> v .: "marks"
      _ -> fail "Unknown span type"
