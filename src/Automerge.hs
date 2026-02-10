{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Automerge (parseAutomergeSpans, parseAutomergeSpansText, Span (..), BlockMarker (..), CodeBlock (..), CodeBlockLanguage (..), Heading (..), HeadingLevel (..), NoteId (..), BlockSpan (..), BlockType (..), TextSpan (..), Mark (..), Link (..), toJSONText, takeUntilNonEmbedBlockSpan, takeUntilNextSameBlockTypeSibling, isTopLevelBlock, isParent, isSiblingListItem) where

import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (Bool, Null, String), eitherDecode, eitherDecodeStrictText, encode, object, withObject, withScientific, withText, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.List (unsnoc)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Utils.JSON (parseNonEmpty, parseStringifiedObject, stringifyObject)

-- TODO: Make title optional
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
  | Code
  deriving (Show, Eq)

newtype HeadingLevel = HeadingLevel Int deriving (Show, Eq)

instance FromJSON HeadingLevel where
  parseJSON = withScientific "HeadingLevel" $ \n -> do
    let level = floor n
    if level >= 1 && level <= 6
      then pure $ HeadingLevel level
      else fail "Invalid heading level"

newtype Heading = Heading HeadingLevel deriving (Show, Eq)

newtype CodeBlockLanguage = CodeBlockLanguage T.Text deriving (Show, Eq)

instance FromJSON CodeBlockLanguage where
  parseJSON = withText "CodeBlockLanguage" $ \language -> pure $ CodeBlockLanguage language

newtype CodeBlock = CodeBlock (Maybe CodeBlockLanguage) deriving (Show, Eq)

newtype NoteId = NoteId T.Text deriving (Show, Eq)

instance FromJSON NoteId where
  parseJSON = withText "NoteId" $ \t -> do
    noteId <- parseNonEmpty "id" t
    pure $ NoteId noteId

data BlockType
  = ParagraphType
  | HeadingType
  | CodeBlockType
  | BlockQuoteType
  | OrderedListItemType
  | UnorderedListItemType
  | ImageType
  | NoteRefType
  | NoteContentType
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
    "__ext__note_ref" -> pure NoteRefType
    "__ext__note_content" -> pure NoteContentType
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
    NoteRefType -> String "__ext__note_ref"
    NoteContentType -> String "__ext__note_content"

data BlockMarker
  = ParagraphMarker
  | HeadingMarker Heading
  | CodeBlockMarker CodeBlock
  | BlockQuoteMarker
  | OrderedListItemMarker
  | UnorderedListItemMarker
  | ImageBlockMarker
  | NoteRefMarker NoteId
  | NoteContentMarker NoteId
  deriving (Show, Eq)

data TextSpan = AutomergeText {value :: T.Text, marks :: [Mark]} deriving (Show, Eq)

instance Semigroup TextSpan where
  (<>) (AutomergeText value1 marks1) (AutomergeText value2 marks2) = AutomergeText (value1 <> value2) (marks1 <> marks2)

instance Monoid TextSpan where
  mempty = AutomergeText T.empty []

data BlockSpan = AutomergeBlock {blockMarker :: BlockMarker, parentTypes :: [BlockType], isEmbed :: Bool} deriving (Show, Eq)

blockType :: BlockSpan -> BlockType
blockType (AutomergeBlock (ParagraphMarker) _ _) = ParagraphType
blockType (AutomergeBlock (HeadingMarker _) _ _) = HeadingType
blockType (AutomergeBlock (CodeBlockMarker _) _ _) = CodeBlockType
blockType (AutomergeBlock (BlockQuoteMarker) _ _) = BlockQuoteType
blockType (AutomergeBlock (OrderedListItemMarker) _ _) = OrderedListItemType
blockType (AutomergeBlock (UnorderedListItemMarker) _ _) = UnorderedListItemType
blockType (AutomergeBlock (ImageBlockMarker) _ _) = ImageType
blockType (AutomergeBlock (NoteRefMarker _) _ _) = NoteRefType
blockType (AutomergeBlock (NoteContentMarker _) _ _) = NoteContentType

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
  embed <- (blockData .: "isEmbed" :: Parser Bool)
  case bt of
    ParagraphType -> pure $ BlockSpan $ AutomergeBlock ParagraphMarker parents embed
    HeadingType -> do
      attrs <- blockData .: "attrs"
      level <- attrs .: "level"
      pure $ BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel level) parents embed
    CodeBlockType -> do
      attrs <- blockData .: "attrs"
      maybeLanguage <- attrs .:? "language"
      case maybeLanguage of
        Nothing -> pure $ BlockSpan $ AutomergeBlock (CodeBlockMarker $ CodeBlock Nothing) parents embed
        Just language -> pure $ BlockSpan $ AutomergeBlock (CodeBlockMarker $ CodeBlock $ Just (CodeBlockLanguage language)) parents embed
    BlockQuoteType -> pure $ BlockSpan $ AutomergeBlock BlockQuoteMarker parents embed
    OrderedListItemType -> pure $ BlockSpan $ AutomergeBlock OrderedListItemMarker parents embed
    UnorderedListItemType -> pure $ BlockSpan $ AutomergeBlock UnorderedListItemMarker parents embed
    ImageType -> pure $ BlockSpan $ AutomergeBlock ImageBlockMarker parents embed
    NoteRefType -> do
      attrs <- blockData .: "attrs"
      noteId <- attrs .: "id"
      pure $ BlockSpan $ AutomergeBlock (NoteRefMarker noteId) parents embed
    NoteContentType -> do
      attrs <- blockData .: "attrs"
      noteId <- attrs .: "id"
      pure $ BlockSpan $ AutomergeBlock (NoteContentMarker noteId) parents embed

parseInline :: Object -> Parser Span
parseInline v = do
  parsedValue <- v .: "value"
  marksKeyMap <- v .:? "marks" .!= KM.empty
  parsedMarks <- parseMarks marksKeyMap
  pure $ TextSpan $ AutomergeText parsedValue parsedMarks

parseMarks :: KM.KeyMap Value -> Parser [Mark]
parseMarks = mapM parseMark . filterNonNull . KM.toList
  where
    filterNonNull = filter (\(_, v) -> v /= Null)

parseMark :: (K.Key, Value) -> Parser Mark
parseMark (k, String txt)
  | K.toText k == "link" = parseStringifiedObject txt >>= (pure . LinkMark)
parseMark (k, Bool True) = case K.toText k of
  "strong" -> pure Strong
  "em" -> pure Emphasis
  "code" -> pure Code
  _ -> fail $ "Unexpected mark with boolean value: " ++ T.unpack (K.toText k)
parseMark _ = fail "Invalid format in marks"

parseAutomergeSpans :: BL.ByteString -> Either String [Span]
parseAutomergeSpans = eitherDecode

parseAutomergeSpansText :: T.Text -> Either String [Span]
parseAutomergeSpansText = eitherDecodeStrictText

instance ToJSON Span where
  toJSON (BlockSpan (AutomergeBlock marker parents embed)) = case marker of
    ParagraphMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= embed,
                "parents" .= parents,
                "type" .= T.pack "paragraph",
                "attrs" .= toJSON (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    HeadingMarker (Heading (HeadingLevel level)) ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= embed,
                "parents" .= parents,
                "type" .= T.pack "heading",
                "attrs" .= object ["level" .= level]
              ]
        ]
    CodeBlockMarker (CodeBlock (maybeCodeBlockLanguage)) ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= embed,
                "parents" .= parents,
                "type" .= T.pack "code-block",
                "attrs" .= case maybeCodeBlockLanguage of
                  Nothing -> toJSON (KM.empty :: KM.KeyMap T.Text)
                  Just (CodeBlockLanguage language) -> object ["language" .= language]
              ]
        ]
    BlockQuoteMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= embed,
                "parents" .= ([] :: [T.Text]),
                "type" .= T.pack "blockquote",
                "attrs" .= toJSON (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    OrderedListItemMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= embed,
                "parents" .= parents,
                "type" .= T.pack "ordered-list-item",
                "attrs" .= toJSON (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    UnorderedListItemMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= embed,
                "parents" .= parents,
                "type" .= T.pack "unordered-list-item",
                "attrs" .= toJSON (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    ImageBlockMarker ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= embed,
                "parents" .= parents,
                "type" .= T.pack "image",
                "attrs" .= (KM.empty :: KM.KeyMap T.Text)
              ]
        ]
    NoteRefMarker (NoteId noteId) ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= embed,
                "parents" .= parents,
                "type" .= T.pack "__ext__note_ref",
                "attrs" .= object ["id" .= noteId]
              ]
        ]
    NoteContentMarker (NoteId noteId) ->
      object
        [ "type" .= T.pack "block",
          "value"
            .= object
              [ "isEmbed" .= embed,
                "parents" .= parents,
                "type" .= T.pack "__ext__note_content",
                "attrs" .= object ["id" .= noteId]
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
        Code -> (K.fromText "code", Bool True)

toJSONText :: [Span] -> T.Text
toJSONText = decodeUtf8 . BSL8.toStrict . encode

takeUntilNonEmbedBlockSpan :: [Span] -> [Span]
takeUntilNonEmbedBlockSpan [] = []
takeUntilNonEmbedBlockSpan (x : xs) = case x of
  BlockSpan (AutomergeBlock _ _ False) -> []
  _ -> x : takeUntilNonEmbedBlockSpan xs

takeUntilNextSameBlockTypeSibling :: BlockSpan -> [Span] -> [Span]
takeUntilNextSameBlockTypeSibling _ [] = []
takeUntilNextSameBlockTypeSibling bl (x : xs) = case x of
  BlockSpan blockSpan | (isSibling blockSpan bl && blockType blockSpan == blockType bl) -> []
  _ -> x : takeUntilNextSameBlockTypeSibling bl xs

isTopLevelBlock :: BlockSpan -> Bool
isTopLevelBlock (AutomergeBlock _ parents _) = null parents

isParent :: Maybe BlockSpan -> BlockSpan -> Bool
isParent (Just parentBlock) potentialChildBlock = lastParentMatches parentBlockType potentialChildParents && isProperPrefix parentParents potentialChildParents
  where
    parentBlockType = blockType parentBlock
    parentParents = parentTypes parentBlock
    potentialChildParents = parentTypes potentialChildBlock
isParent Nothing blockSpan = isTopLevelBlock blockSpan

isSibling :: BlockSpan -> BlockSpan -> Bool
isSibling (AutomergeBlock _ block1Parents _) (AutomergeBlock _ block2Parents _) = block1Parents == block2Parents

lastParentMatches :: BlockType -> [BlockType] -> Bool
lastParentMatches parentBlockType potentialChildParents = case unsnoc potentialChildParents of
  Nothing -> False
  Just (_, lastParentOfCandidate) -> parentBlockType == lastParentOfCandidate

isSiblingListItem :: BlockSpan -> BlockSpan -> Bool
isSiblingListItem (AutomergeBlock UnorderedListItemMarker listItem1Parents _) (AutomergeBlock UnorderedListItemMarker listItem2Parents _) = listItem1Parents == listItem2Parents
isSiblingListItem (AutomergeBlock OrderedListItemMarker listItem1Parents _) (AutomergeBlock OrderedListItemMarker listItem2Parents _) = listItem1Parents == listItem2Parents
isSiblingListItem (AutomergeBlock _ _ _) (AutomergeBlock _ _ _) = False

isProperPrefix :: [BlockType] -> [BlockType] -> Bool
isProperPrefix _ [] = False
isProperPrefix parents potentialChildParents = parents == (init potentialChildParents)