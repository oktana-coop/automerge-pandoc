{-# LANGUAGE OverloadedStrings #-}

module Automerge (parseAutomergeSpans, AutomergeSpan (..), BlockMarker (..), Heading (..), HeadingLevel (..), TextSpan (..), Mark (..), toJSONText) where

import Data.Aeson (FromJSON (parseJSON), Object (..), ToJSON (toJSON), Value (Bool), eitherDecode, encode, object, withObject, withScientific, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

data Mark
  = Strong
  | Emphasis
  | Link {url :: T.Text, title :: T.Text}
  deriving (Show, Eq)

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

data TextSpan = AutomergeText {value :: T.Text, marks :: [Mark]} deriving (Show, Eq)

instance Semigroup TextSpan where
  (<>) (AutomergeText value1 marks1) (AutomergeText value2 marks2) = AutomergeText (value1 <> value2) (marks1 <> marks2)

instance Monoid TextSpan where
  mempty = AutomergeText T.empty []

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

parseMarks :: KM.KeyMap Value -> Parser [Mark]
parseMarks = mapM parseMark . KM.toList

parseMark :: (K.Key, Value) -> Parser Mark
parseMark (k, Object o)
  | K.toText == "link" = parseLink o
parseMark (k, Bool True) = case K.toText k of
  "strong" -> Just Strong
  "em" -> Just Emphasis
  _ -> fail $ "Unexpected mark with boolean value: " ++ T.unpack (K.toText k)
parseMark _ = fail "Invalid format in marks"

parseNonEmpty :: String -> T.Text -> Parser T.Text
parseNonEmpty fieldName txt
  | T.null txt = fail $ fieldname ++ " must be non-empty"
  | otherwise = pure txt

parseLink :: Object -> Parser Mark
parseLink v = do
  url <- v .: "href" >>= parseNonEmpty "href"
  title <- v .: "title" >>= parseNonEmpty "title"
  pure $ Link {url = url, title = title}

parseAutomergeSpans :: BL.ByteString -> Either String [AutomergeSpan]
parseAutomergeSpans = eitherDecode

instance ToJSON AutomergeSpan where
  toJSON (BlockSpan blockMarker) = case blockMarker of
    ParagraphMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= ([] :: [T.Text]),
                "type" .= T.pack "paragraph",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    HeadingMarker (Heading (HeadingLevel level)) ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= ([] :: [T.Text]),
                "type" .= T.pack "heading",
                "attrs" .= object ["level" .= level]
              ]
        ]
    CodeBlockMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= ([] :: [T.Text]),
                "type" .= T.pack "code-block",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    BlockQuoteMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= ([] :: [T.Text]),
                "type" .= T.pack "blockquote",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    OrderedListItemMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool True,
                "parents" .= ([] :: [T.Text]),
                "type" .= T.pack "ordered-list-item",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    UnorderedListItemMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= ([] :: [T.Text]),
                "type" .= T.pack "unordered-list-item",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    ImageBlockMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= ([] :: [T.Text]),
                "type" .= T.pack "image",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
  toJSON (TextSpan (AutomergeText val extractedMarks)) =
    object $
      [ "type" .= T.pack "text",
        "value" .= val
      ]
        <> ["marks" .= KM.fromList (map markToKeyVal extractedMarks) | not (null extractedMarks)]
    where
      markToKeyVal mark = case mark of
        Strong -> (K.fromText "strong", Bool True)
        Emphasis -> (K.fromText "em", Bool True)
        Link linkUrl linkTitle -> (K.fromText "link", object ["href" .= linkUrl, "title" .= linkTitle])

toJSONText :: [AutomergeSpan] -> T.Text
toJSONText = decodeUtf8 . BSL8.toStrict . encode
