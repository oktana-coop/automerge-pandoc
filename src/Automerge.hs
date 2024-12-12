{-# LANGUAGE OverloadedStrings #-}

module Automerge (parseAutomergeSpans) where

import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    eitherDecode,
    withObject,
    withScientific,
    (.:),
    (.:?),
  )
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, Value (Bool), (.!=))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

data Mark
  = Strong
  | Emphasis
  | Link
  deriving (Show)

-- TODO: Constrain to levels 1-6
newtype HeadingLevel = HeadingLevel Int deriving (Show)

instance FromJSON HeadingLevel where
  parseJSON = withScientific "HeadingLevel" $ \n -> do
    let level = floor n
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
      attrs <- blockData .: "attrs"
      level <- attrs .: "level"
      pure $ Block $ HeadingSpan $ Heading $ HeadingLevel level
    "code-block" -> pure $ Block CodeBlockSpan
    "blockquote" -> pure $ Block BlockQuoteSpan
    "ordered-list-item" -> pure $ Block OrderedListItemSpan
    "unordered-list-item" -> pure $ Block UnorderedListItemSpan
    "image" -> pure $ Block ImageBlockSpan
    _ -> fail "Invalid block type"

parseInline :: Object -> Parser AutomergeSpan
parseInline v = do
  parsedValue <- v .: "value"
  marksKeyMap <- v .:? "marks" .!= KM.empty
  let parsedMarks = parseMarks marksKeyMap
  pure $ Inline $ TextSpan parsedValue parsedMarks

parseMarks :: KM.KeyMap Value -> [Mark]
parseMarks = mapMaybe parseMark . KM.toList
  where
    parseMark (k, Bool True) = case K.toText k of
      "strong" -> Just Strong
      "em" -> Just Emphasis
      "link" -> Just Link
      _ -> Nothing
    parseMark _ = Nothing

parseAutomergeSpans :: BL.ByteString -> Either String [AutomergeSpan]
parseAutomergeSpans = eitherDecode
