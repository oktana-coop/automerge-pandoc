module AutomergeTestUtils (paragraphSpan, heading1Span, heading2Span, heading3Span, heading4Span, heading5Span, heading6Span, textSpan, strongTextSpan, emphasisTextSpan, codeTextSpan, textSpanWithMarks, linkTextSpan, codeBlockSpan, blockQuoteSpan, orderedListItemSpan, unorderedListItemSpan) where

import Automerge (BlockMarker (..), BlockSpan (..), BlockType (..), Heading (..), HeadingLevel (..), Link (..), Mark (..), Span (..), TextSpan (..))
import qualified Data.Text as T

paragraphSpan :: [BlockType] -> Span
paragraphSpan parents = BlockSpan $ AutomergeBlock ParagraphMarker parents

heading1Span :: [BlockType] -> Span
heading1Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 1) parents

heading2Span :: [BlockType] -> Span
heading2Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 2) parents

heading3Span :: [BlockType] -> Span
heading3Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 3) parents

heading4Span :: [BlockType] -> Span
heading4Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 4) parents

heading5Span :: [BlockType] -> Span
heading5Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 5) parents

heading6Span :: [BlockType] -> Span
heading6Span parents = BlockSpan $ AutomergeBlock (HeadingMarker $ Heading $ HeadingLevel 6) parents

textSpan :: String -> Span
textSpan str = TextSpan $ AutomergeText (T.pack str) []

strongTextSpan :: String -> Span
strongTextSpan str = TextSpan $ AutomergeText (T.pack str) [Strong]

emphasisTextSpan :: String -> Span
emphasisTextSpan str = TextSpan $ AutomergeText (T.pack str) [Emphasis]

linkTextSpan :: String -> String -> String -> Span
linkTextSpan txt linkUrl linkTitle = TextSpan $ AutomergeText (T.pack txt) [LinkMark $ Link (T.pack linkUrl) (T.pack linkTitle)]

codeTextSpan :: String -> Span
codeTextSpan str = TextSpan $ AutomergeText (T.pack str) [Code]

textSpanWithMarks :: String -> [Mark] -> Span
textSpanWithMarks str spanMarks = TextSpan $ AutomergeText (T.pack str) spanMarks

unorderedListItemSpan :: [BlockType] -> Span
unorderedListItemSpan parents = BlockSpan $ AutomergeBlock UnorderedListItemMarker parents

orderedListItemSpan :: [BlockType] -> Span
orderedListItemSpan parents = BlockSpan $ AutomergeBlock OrderedListItemMarker parents

codeBlockSpan :: [BlockType] -> Span
codeBlockSpan parents = BlockSpan $ AutomergeBlock CodeBlockMarker parents

blockQuoteSpan :: [BlockType] -> Span
blockQuoteSpan parents = BlockSpan $ AutomergeBlock BlockQuoteMarker parents
