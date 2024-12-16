module PandocWriter (writeAutomergeSpans) where

import Automerge (AutomergeSpan (..), BlockMarker (..), Heading (..), HeadingLevel (..), Mark (..), TextSpan (..), toJSONText)
import qualified Data.Text as T
import Text.Pandoc (WriterOptions)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (Pandoc))

writeAutomergeSpans :: (PandocMonad m) => WriterOptions -> Pandoc -> m T.Text
writeAutomergeSpans _ (Pandoc _ blocks) =
  pure $ toJSONText $ blocksToAutomergeSpans blocks

blocksToAutomergeSpans :: [Block] -> [AutomergeSpan]
blocksToAutomergeSpans = concatMap blockToAutomergeSpans

blockToAutomergeSpans :: Block -> [AutomergeSpan]
blockToAutomergeSpans block = case block of
  Para inlines -> [BlockSpan ParagraphMarker, TextSpan (inlinesToAutomergeTextSpan inlines)]
  Header level _ inlines -> [BlockSpan $ HeadingMarker $ Heading $ HeadingLevel level, TextSpan (inlinesToAutomergeTextSpan inlines)]
  CodeBlock _ text -> [BlockSpan CodeBlockMarker, TextSpan $ AutomergeText text []]
  -- TODO: Implement blockquote, which contains a list of blocks in Pandoc
  _ -> [] -- Ignore blocks we don't recognize. TODO: Implement something more sophisticated here.

inlinesToAutomergeTextSpan :: [Inline] -> TextSpan
inlinesToAutomergeTextSpan = foldMap inlineToTextSpan

inlineToTextSpan :: Inline -> TextSpan
inlineToTextSpan inline = case inline of
  Str str -> AutomergeText str []
  Text.Pandoc.Definition.Strong inlines -> addMarkTo (inlinesToAutomergeTextSpan inlines) Automerge.Strong
  Text.Pandoc.Definition.Emph inlines -> addMarkTo (inlinesToAutomergeTextSpan inlines) Automerge.Emphasis
  -- TODO: Handle other inline elements
  _ -> AutomergeText T.empty []

addMarkTo :: TextSpan -> Mark -> TextSpan
addMarkTo textSpan mark = AutomergeText (value textSpan) (marks textSpan ++ [mark])