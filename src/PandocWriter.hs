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
  Para inlines -> BlockSpan ParagraphMarker : (TextSpan <$> inlinesToAutomergeTextSpans inlines)
  Header level _ inlines -> BlockSpan (HeadingMarker $ Heading $ HeadingLevel level) : (TextSpan <$> inlinesToAutomergeTextSpans inlines)
  CodeBlock _ text -> [BlockSpan CodeBlockMarker, TextSpan $ AutomergeText text []]
  -- TODO: Implement blockquote, which contains a list of blocks in Pandoc
  _ -> [] -- Ignore blocks we don't recognize. TODO: Implement something more sophisticated here.

inlinesToAutomergeTextSpans :: [Inline] -> [TextSpan]
inlinesToAutomergeTextSpans = foldMap inlineToTextSpan

inlineToTextSpan :: Inline -> [TextSpan]
inlineToTextSpan inline = case inline of
  Str str -> [AutomergeText str []]
  Text.Pandoc.Definition.Strong inlines -> addMark Automerge.Strong inlines
  Text.Pandoc.Definition.Emph inlines -> addMark Automerge.Emphasis inlines
  -- TODO: Handle other inline elements
  _ -> []

addMark :: Mark -> [Inline] -> [TextSpan]
-- Monoidally add the mark to all text spans created for the inline elements
addMark mark inlines = fmap (AutomergeText T.empty [mark] <>) (inlinesToAutomergeTextSpans inlines)
