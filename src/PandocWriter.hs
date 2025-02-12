module PandocWriter (writeAutomergeSpans) where

import Automerge (BlockMarker (..), BlockSpan (..), Heading (..), HeadingLevel (..), Link (..), Mark (..), Span (..), TextSpan (..), toJSONText)
import qualified Data.Text as T
import Text.Pandoc (WriterOptions)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition as Pandoc (Block (..), Inline (..), Pandoc (Pandoc))

writeAutomergeSpans :: (PandocMonad m) => WriterOptions -> Pandoc.Pandoc -> m T.Text
writeAutomergeSpans _ (Pandoc.Pandoc _ blocks) =
  pure $ toJSONText $ blocksToAutomergeSpans blocks

blocksToAutomergeSpans :: [Pandoc.Block] -> [Automerge.Span]
blocksToAutomergeSpans = concatMap blockToAutomergeSpans

blockToAutomergeSpans :: Pandoc.Block -> [Automerge.Span]
blockToAutomergeSpans block = case block of
  Pandoc.Para inlines -> (Automerge.BlockSpan $ AutomergeBlock ParagraphMarker []) : (Automerge.TextSpan <$> inlinesToAutomergeTextSpans inlines)
  Pandoc.Header level _ inlines -> (Automerge.BlockSpan $ AutomergeBlock (Automerge.HeadingMarker $ Heading $ HeadingLevel level) []) : (Automerge.TextSpan <$> inlinesToAutomergeTextSpans inlines)
  Pandoc.CodeBlock _ text -> [Automerge.BlockSpan $ AutomergeBlock Automerge.CodeBlockMarker [], Automerge.TextSpan $ AutomergeText text []]
  -- TODO: Implement blockquote, which contains a list of blocks in Pandoc
  _ -> [] -- Ignore blocks we don't recognize. TODO: Implement something more sophisticated here.

inlinesToAutomergeTextSpans :: [Pandoc.Inline] -> [Automerge.TextSpan]
inlinesToAutomergeTextSpans = mergeSameMarkSpans . foldMap inlineToTextSpan

mergeSameMarkSpans :: [Automerge.TextSpan] -> [Automerge.TextSpan]
mergeSameMarkSpans = foldr mergeOrAppendAdjacent []

-- This is the folding function for merging the adjacent elements if their marks are the same
mergeOrAppendAdjacent :: Automerge.TextSpan -> [Automerge.TextSpan] -> [Automerge.TextSpan]
mergeOrAppendAdjacent x [] = [x]
-- pattern-match on: the current element (x), the one to its right (firstOfRest) and the rest of the fold
mergeOrAppendAdjacent x (firstOfRest : rest) =
  if marks x == marks firstOfRest
    -- if the element's marks are the same with the one to its right, we merge them and then add them to the rest of the fold.
    then (x <> firstOfRest) : rest
    -- if they are not the same we end up with an extra text span in the list for the current element (we prepend it to the existing list for the fold.)
    else x : firstOfRest : rest

inlineToTextSpan :: Pandoc.Inline -> [Automerge.TextSpan]
inlineToTextSpan inline = case inline of
  Pandoc.Str str -> [AutomergeText str []]
  Pandoc.Space -> [AutomergeText (T.pack " ") []]
  Pandoc.Strong inlines -> addMark Automerge.Strong inlines
  Pandoc.Emph inlines -> addMark Automerge.Emphasis inlines
  Pandoc.Link _ inlines (linkUrl, linkTitle) -> addMark (Automerge.LinkMark $ Automerge.Link {url = linkUrl, title = linkTitle}) inlines
  -- TODO: Handle other inline elements
  _ -> []

addMark :: Automerge.Mark -> [Pandoc.Inline] -> [Automerge.TextSpan]
-- Monoidally add the mark to all text spans created for the inline elements
addMark mark inlines = fmap (AutomergeText T.empty [mark] <>) (inlinesToAutomergeTextSpans inlines)
