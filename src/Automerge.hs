{-# LANGUAGE OverloadedStrings #-}

module Automerge (parseAutomergeSpans, AutomergeSpan (..), BlockMarker (..), Heading (..), HeadingLevel (..), TextSpan) where

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

data BlockMarker
  = ParagraphMarker
  | HeadingMarker Heading
  | CodeBlockMarker
  | BlockQuoteMarker
  | OrderedListItemMarker
  | UnorderedListItemMarker
  | ImageBlockMarker
  deriving (Show)

data TextSpan = AutomergeText {value :: T.Text, marks :: [Mark]} deriving (Show)

data AutomergeSpan
  = BlockSpan BlockMarker
  | TextSpan TextSpan
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
    "paragraph" -> pure $ BlockSpan ParagraphMarker
    "heading" -> do
      attrs <- blockData .: "attrs"
      level <- attrs .: "level"
      pure $ BlockSpan $ HeadingMarker $ Heading $ HeadingLevel level
    "code-block" -> pure $ BlockSpan CodeBlockMarker
    "blockquote" -> pure $ BlockSpan BlockQuoteMarker
    "ordered-list-item" -> pure $ BlockSpan OrderedListItemMarker
    "unordered-list-item" -> pure $ BlockSpan UnorderedListItemMarker
    "image" -> pure $ BlockSpan ImageBlockMarker
    _ -> fail "Invalid block type"

parseInline :: Object -> Parser AutomergeSpan
parseInline v = do
  parsedValue <- v .: "value"
  marksKeyMap <- v .:? "marks" .!= KM.empty
  let parsedMarks = parseMarks marksKeyMap
  pure $ TextSpan $ AutomergeText parsedValue parsedMarks

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
