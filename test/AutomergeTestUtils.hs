module AutomergeTestUtils (paragraphSpan, heading1Span, heading2Span, heading3Span, heading4Span, heading5Span, heading6Span, textSpan, strongTextSpan, emphasisTextSpan, codeTextSpan, textSpanWithMarks, linkTextSpan, codeBlockSpan, blockQuoteSpan, orderedListItemSpan, unorderedListItemSpan, horizontalRuleSpan, noteRefSpan, noteContentSpan, figureSpan, captionSpan, imageSpan) where

import Automerge (BlockMarker (..), BlockSpan (..), BlockType (..), CodeBlock (CodeBlock), Heading (..), HeadingLevel (..), Image (..), Link (..), Mark (..), NoteId (..), Span (..), TextSpan (..))
import qualified Data.Text as T

paragraphSpan :: [BlockType] -> Span
paragraphSpan parents = BlockSpan $ AutomergeBlock ParagraphMarker parents False

heading1Span :: [BlockType] -> Span
heading1Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 1) parents False

heading2Span :: [BlockType] -> Span
heading2Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 2) parents False

heading3Span :: [BlockType] -> Span
heading3Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 3) parents False

heading4Span :: [BlockType] -> Span
heading4Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 4) parents False

heading5Span :: [BlockType] -> Span
heading5Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 5) parents False

heading6Span :: [BlockType] -> Span
heading6Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 6) parents False

textSpan :: String -> Span
textSpan str = TextSpan $ AutomergeText (T.pack str) []

strongTextSpan :: String -> Span
strongTextSpan str = TextSpan $ AutomergeText (T.pack str) [Strong]

emphasisTextSpan :: String -> Span
emphasisTextSpan str = TextSpan $ AutomergeText (T.pack str) [Emphasis]

linkTextSpan :: String -> String -> String -> Span
linkTextSpan txt linkUrl linkTtl = TextSpan $ AutomergeText (T.pack txt) [LinkMark $ Link (T.pack linkUrl) (T.pack linkTtl)]

codeTextSpan :: String -> Span
codeTextSpan str = TextSpan $ AutomergeText (T.pack str) [Code]

textSpanWithMarks :: String -> [Mark] -> Span
textSpanWithMarks str spanMarks = TextSpan $ AutomergeText (T.pack str) spanMarks

unorderedListItemSpan :: [BlockType] -> Span
unorderedListItemSpan parents = BlockSpan $ AutomergeBlock UnorderedListItemMarker parents False

orderedListItemSpan :: [BlockType] -> Span
orderedListItemSpan parents = BlockSpan $ AutomergeBlock OrderedListItemMarker parents False

codeBlockSpan :: [BlockType] -> Span
codeBlockSpan parents = BlockSpan $ AutomergeBlock (CodeBlockMarker $ CodeBlock Nothing) parents False

blockQuoteSpan :: [BlockType] -> Span
blockQuoteSpan parents = BlockSpan $ AutomergeBlock BlockQuoteMarker parents False

horizontalRuleSpan :: [BlockType] -> Span
horizontalRuleSpan parents = BlockSpan $ AutomergeBlock HorizontalRuleMarker parents False

noteRefSpan :: [BlockType] -> String -> Span
noteRefSpan parents str = BlockSpan $ AutomergeBlock (NoteRefMarker (NoteId $ T.pack str)) parents True

noteContentSpan :: [BlockType] -> String -> Span
noteContentSpan parents str = BlockSpan $ AutomergeBlock {blockMarker = (NoteContentMarker (NoteId $ T.pack str)), parentTypes = parents, isEmbed = False}

figureSpan :: [BlockType] -> Span
figureSpan parents = BlockSpan $ AutomergeBlock FigureMarker parents False

captionSpan :: [BlockType] -> Span
captionSpan parents = BlockSpan $ AutomergeBlock CaptionMarker parents False

imageSpan :: [BlockType] -> String -> Maybe String -> Maybe String -> Span
imageSpan parents imgSrc imgTitle imgAlt =
  BlockSpan $ AutomergeBlock (ImageMarker $ Image (T.pack imgSrc) (T.pack <$> imgTitle) (T.pack <$> imgAlt)) parents True
