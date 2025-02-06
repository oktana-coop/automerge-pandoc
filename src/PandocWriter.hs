module PandocWriter (writeAutomergeSpans) where

import Automerge (AutomergeSpan (..), BlockMarker (..), Heading (..), HeadingLevel (..), Link (..), Mark (..), TextSpan (..), toJSONText)
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
  Para inlines -> BlockSpan ParagraphMarker [] : (TextSpan <$> inlinesToAutomergeTextSpans inlines)
  Header level _ inlines -> BlockSpan (HeadingMarker $ Heading $ HeadingLevel level) [] : (TextSpan <$> inlinesToAutomergeTextSpans inlines)
  CodeBlock _ text -> [BlockSpan CodeBlockMarker [], TextSpan $ AutomergeText text []]
  -- TODO: Implement blockquote, which contains a list of blocks in Pandoc
  _ -> [] -- Ignore blocks we don't recognize. TODO: Implement something more sophisticated here.

inlinesToAutomergeTextSpans :: [Inline] -> [TextSpan]
inlinesToAutomergeTextSpans = mergeSameMarkSpans . foldMap inlineToTextSpan

mergeSameMarkSpans :: [TextSpan] -> [TextSpan]
mergeSameMarkSpans = foldr mergeOrAppendAdjacent []

-- This is the folding function for merging the adjacent elements if their marks are the same
mergeOrAppendAdjacent :: TextSpan -> [TextSpan] -> [TextSpan]
mergeOrAppendAdjacent x [] = [x]
-- pattern-match on: the current element (x), the one to its right (firstOfRest) and the rest of the fold
mergeOrAppendAdjacent x (firstOfRest : rest) =
  if marks x == marks firstOfRest
    -- if the element's marks are the same with the one to its right, we merge them and then add them to the rest of the fold.
    then (x <> firstOfRest) : rest
    -- if they are not the same we end up with an extra text span in the list for the current element (we prepend it to the existing list for the fold.)
    else x : firstOfRest : rest

inlineToTextSpan :: Inline -> [TextSpan]
inlineToTextSpan inline = case inline of
  Str str -> [AutomergeText str []]
  Space -> [AutomergeText (T.pack " ") []]
  Text.Pandoc.Definition.Strong inlines -> addMark Automerge.Strong inlines
  Text.Pandoc.Definition.Emph inlines -> addMark Automerge.Emphasis inlines
  Text.Pandoc.Definition.Link _ inlines (linkUrl, linkTitle) -> addMark (Automerge.LinkMark $ Automerge.Link {url = linkUrl, title = linkTitle}) inlines
  -- TODO: Handle other inline elements
  _ -> []

addMark :: Mark -> [Inline] -> [TextSpan]
-- Monoidally add the mark to all text spans created for the inline elements
addMark mark inlines = fmap (AutomergeText T.empty [mark] <>) (inlinesToAutomergeTextSpans inlines)
