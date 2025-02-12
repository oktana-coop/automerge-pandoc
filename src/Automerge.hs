{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Automerge (parseAutomergeSpans, Span (..), BlockMarker (..), Heading (..), HeadingLevel (..), BlockSpan (..), TextSpan (..), Mark (..), Link (..), toJSONText, takeUntilBlockSpan, isTopLevelBlock, isParent, isSiblingListItem) where

import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (Bool, String), eitherDecode, encode, object, withObject, withScientific, withText, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.List (unsnoc)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Utils.JSON (parseNonEmpty, parseStringifiedObject, stringifyObject)

data Link = Link {url :: T.Text, title :: T.Text} deriving (Show, Eq)

instance FromJSON Link where
  parseJSON = withObject "Link" $ \v -> do
    linkUrl <- v .: "href" >>= parseNonEmpty "href"
    linkTitle <- v .: "title"
    pure Link {url = linkUrl, title = linkTitle}

instance ToJSON Link where
  toJSON link = object ["href" .= url link, "title" .= title link]

data Mark
  = Strong
  | Emphasis
  | LinkMark Link
  deriving (Show, Eq)

newtype HeadingLevel = HeadingLevel Int deriving (Show, Eq)

instance FromJSON HeadingLevel where
  parseJSON = withScientific "HeadingLevel" $ \n -> do
    let level = floor n
    if level >= 1 && level <= 6
      then pure $ HeadingLevel level
      else fail "Invalid heading level"

newtype Heading = Heading HeadingLevel deriving (Show, Eq)

data BlockType
  = ParagraphType
  | HeadingType
  | CodeBlockType
  | BlockQuoteType
  | OrderedListItemType
  | UnorderedListItemType
  | ImageType
  deriving (Show, Eq)

instance FromJSON BlockType where
  parseJSON :: Value -> Parser BlockType
  parseJSON = withText "BlockType" $ \t -> case t of
    "paragraph" -> pure ParagraphType
    "heading" -> pure HeadingType
    "code-block" -> pure CodeBlockType
    "blockquote" -> pure BlockQuoteType
    "ordered-list-item" -> pure OrderedListItemType
    "unordered-list-item" -> pure UnorderedListItemType
    "image" -> pure ImageType
    _ -> fail "Invalid block type"

instance ToJSON BlockType where
  toJSON :: BlockType -> Value
  toJSON bt = case bt of
    ParagraphType -> String "paragraph"
    HeadingType -> String "heading"
    CodeBlockType -> String "code-block"
    BlockQuoteType -> String "blockquote"
    OrderedListItemType -> String "ordered-list-item"
    UnorderedListItemType -> String "unordered-list-item"
    ImageType -> String "image"

data BlockMarker
  = ParagraphMarker
  | HeadingMarker Heading
  | CodeBlockMarker
  | BlockQuoteMarker
  | OrderedListItemMarker
  | UnorderedListItemMarker
  | ImageBlockMarker
  deriving (Show, Eq)

data TextSpan = AutomergeText {value :: T.Text, marks :: [Mark]} deriving (Show, Eq)

instance Semigroup TextSpan where
  (<>) (AutomergeText value1 marks1) (AutomergeText value2 marks2) = AutomergeText (value1 <> value2) (marks1 <> marks2)

instance Monoid TextSpan where
  mempty = AutomergeText T.empty []

data BlockSpan = AutomergeBlock BlockMarker [BlockType] deriving (Show, Eq)

blockType :: BlockSpan -> BlockType
blockType (AutomergeBlock (ParagraphMarker) _) = ParagraphType
blockType (AutomergeBlock (HeadingMarker _) _) = HeadingType
blockType (AutomergeBlock (CodeBlockMarker) _) = CodeBlockType
blockType (AutomergeBlock (BlockQuoteMarker) _) = BlockQuoteType
blockType (AutomergeBlock (OrderedListItemMarker) _) = OrderedListItemType
blockType (AutomergeBlock (UnorderedListItemMarker) _) = UnorderedListItemType
blockType (AutomergeBlock (ImageBlockMarker) _) = ImageType

data Span
  = BlockSpan BlockSpan
  | TextSpan TextSpan
  deriving (Show, Eq)

instance FromJSON Span where
  parseJSON = withObject "AutomergeSpan" $ \v -> do
    elementType <- (v .: "type" :: Parser String)
    case elementType of
      "block" -> parseBlock v
      "text" -> parseInline v
      _ -> fail "Unknown span type"

parseBlock :: Object -> Parser Span
parseBlock v = do
  blockData <- v .: "value"
  bt <- (blockData .: "type" :: Parser BlockType)
  parents <- (blockData .: "parents" :: Parser [BlockType])
  case bt of
    ParagraphType -> pure $ BlockSpan $ AutomergeBlock ParagraphMarker parents
    HeadingType -> do
      attrs <- blockData .: "attrs"
      level <- attrs .: "level"
      pure $ BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel level) parents
    CodeBlockType -> pure $ BlockSpan $ AutomergeBlock CodeBlockMarker parents
    BlockQuoteType -> pure $ BlockSpan $ AutomergeBlock BlockQuoteMarker parents
    OrderedListItemType -> pure $ BlockSpan $ AutomergeBlock OrderedListItemMarker parents
    UnorderedListItemType -> pure $ BlockSpan $ AutomergeBlock UnorderedListItemMarker parents
    ImageType -> pure $ BlockSpan $ AutomergeBlock ImageBlockMarker parents

parseInline :: Object -> Parser Span
parseInline v = do
  parsedValue <- v .: "value"
  marksKeyMap <- v .:? "marks" .!= KM.empty
  parsedMarks <- parseMarks marksKeyMap
  pure $ TextSpan $ AutomergeText parsedValue parsedMarks

parseMarks :: KM.KeyMap Value -> Parser [Mark]
parseMarks = mapM parseMark . KM.toList

parseMark :: (K.Key, Value) -> Parser Mark
parseMark (k, String txt)
  | K.toText k == "link" = parseStringifiedObject txt >>= (pure . LinkMark)
parseMark (k, Bool True) = case K.toText k of
  "strong" -> pure Strong
  "em" -> pure Emphasis
  _ -> fail $ "Unexpected mark with boolean value: " ++ T.unpack (K.toText k)
parseMark _ = fail "Invalid format in marks"

parseAutomergeSpans :: BL.ByteString -> Either String [Span]
parseAutomergeSpans = eitherDecode

instance ToJSON Span where
  toJSON (BlockSpan (AutomergeBlock blockMarker parents)) = case blockMarker of
    ParagraphMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= Bool False,
                "parents" .= parents,
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
                "parents" .= parents,
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
                "parents" .= parents,
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
                "parents" .= parents,
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
                "parents" .= parents,
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
                "parents" .= parents,
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
        LinkMark link -> (K.fromText "link", String $ stringifyObject link)

toJSONText :: [Span] -> T.Text
toJSONText = decodeUtf8 . BSL8.toStrict . encode

takeUntilBlockSpan :: [Span] -> [Span]
takeUntilBlockSpan [] = []
takeUntilBlockSpan (x : xs) = case x of
  BlockSpan _ -> []
  _ -> x : takeUntilBlockSpan xs

isTopLevelBlock :: BlockSpan -> Bool
isTopLevelBlock (AutomergeBlock _ parents) = null parents

isParent :: Maybe BlockSpan -> BlockSpan -> Bool
isParent (Just block@(AutomergeBlock _ parents)) (AutomergeBlock _ candidateParents) = candidateLastParentMatches (blockType block) candidateParents && isProperPrefix parents candidateParents
isParent Nothing blockSpan = isTopLevelBlock blockSpan

candidateLastParentMatches :: BlockType -> [BlockType] -> Bool
candidateLastParentMatches parentBlockType potentialChildParents = case unsnoc potentialChildParents of
  Nothing -> False
  Just (_, lastParentOfCandidate) -> parentBlockType == lastParentOfCandidate

isSiblingListItem :: BlockSpan -> BlockSpan -> Bool
isSiblingListItem (AutomergeBlock UnorderedListItemMarker parents) (AutomergeBlock UnorderedListItemMarker candidateParents) = parents == candidateParents
isSiblingListItem (AutomergeBlock OrderedListItemMarker parents) (AutomergeBlock OrderedListItemMarker candidateParents) = parents == candidateParents
isSiblingListItem (AutomergeBlock _ _) (AutomergeBlock _ _) = False

isProperPrefix :: [BlockType] -> [BlockType] -> Bool
isProperPrefix _ [] = False
isProperPrefix parents potentialChildParents = parents == (init potentialChildParents)