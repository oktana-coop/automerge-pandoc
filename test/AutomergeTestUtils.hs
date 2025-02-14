module AutomergeTestUtils (paragraphSpan, heading1Span, heading2Span, heading3Span, heading4Span, heading5Span, heading6Span, textSpan, strongTextSpan, emphasisTextSpan, textSpanWithMarks, linkTextSpan) where

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

linkTextSpan :: String -> String -> Span
linkTextSpan linkUrl linkTitle = TextSpan $ AutomergeText (T.pack linkTitle) [LinkMark $ Link (T.pack linkUrl) (T.pack linkTitle)]

textSpanWithMarks :: String -> [Mark] -> Span
textSpanWithMarks str spanMarks = TextSpan $ AutomergeText (T.pack str) spanMarks
