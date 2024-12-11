{-# LANGUAGE OverloadedStrings #-}

module Automerge where

import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    withObject,
    withText,
    (.:),
  )
import Data.Aeson.Types (Parser)
import qualified Data.Text as T

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

-- TODO: Constrain to levels 1-6
newtype HeadingLevel = HeadingLevel Int deriving (Show)

instance FromJSON HeadingLevel where
  parseJSON = withObject "HeadingLevel" $ \v -> do
    level <- (v .: "level" :: Parser Int)
    if level >= 1 && level <= 6
      then pure $ HeadingLevel level
      else fail "Invalid heading level"

newtype Heading = Heading HeadingLevel deriving (Show)

data BlockSpan
  = ParagraphSpan
  | HeadingSpan Heading
  | CodeBlockSpan
  | BlockQuoteSpan
  | OrderedListItemSpan
  | UnorderedListItemSpan
  | ImageBlockSpan
  deriving (Show)

data TextSpan = TextSpan {value :: T.Text, marks :: [Mark]} deriving (Show)

data AutomergeSpan
  = Block BlockSpan
  | Inline TextSpan
  deriving (Show)

instance FromJSON AutomergeSpan where
  parseJSON = withObject "AutomergeSpan" $ \v -> do
    elementType <- (v .: "type" :: Parser String)
    case elementType of
      "block" -> parseBlock v
      "text" -> parseInline v
      _ -> fail "Unknown span type"

parseBlock :: Object -> Parser AutomergeSpan
parseBlock v = do
  blockData <- v .: "value"
  blockType <- (blockData .: "type" :: Parser String)
  case blockType of
    "paragraph" -> pure $ Block ParagraphSpan
    "heading" -> do
      level <- blockData .: "level"
      pure $ Block $ HeadingSpan $ Heading $ HeadingLevel level
    "code-block" -> pure $ Block CodeBlockSpan
    "blockquote" -> pure $ Block BlockQuoteSpan
    "ordered-list-item" -> pure $ Block OrderedListItemSpan
    "unordered-list-item" -> pure $ Block UnorderedListItemSpan
    "image" -> pure $ Block ImageBlockSpan
    _ -> fail "Invalid block type"

parseInline :: Object -> Parser AutomergeSpan
parseInline v = Inline <$> (TextSpan <$> v .: "value" <*> v .: "marks")
