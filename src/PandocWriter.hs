module PandocWriter (writeAutomerge) where

import Automerge (BlockMarker (..), BlockSpan (..), BlockType (..), Heading (..), HeadingLevel (..), Link (..), Mark (..), NoteId (..), Span (..), TextSpan (..), toJSONText)
import Control.Monad.State (State, get, modify, runState)
import qualified Data.Text as T
import Text.Pandoc (WriterOptions)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition as Pandoc (Block (..), Inline (..), Pandoc (Pandoc))

data ContainerBlockType = BulletListItem | OrderedListItem | BlockQuote deriving (Show, Eq)

data NoteData = NoteData
  { noteCounter :: Int,
    -- Accumulated note content spans
    noteContents :: [Automerge.Span]
  }

type NotesState = State NoteData

toAutomergeBlockType :: ContainerBlockType -> BlockType
toAutomergeBlockType BulletListItem = Automerge.UnorderedListItemType
toAutomergeBlockType OrderedListItem = Automerge.OrderedListItemType
toAutomergeBlockType PandocWriter.BlockQuote = Automerge.BlockQuoteType

writeAutomerge :: (PandocMonad m) => WriterOptions -> Pandoc.Pandoc -> m T.Text
writeAutomerge _ (Pandoc.Pandoc _ blocks) = pure $ toJSONText allSpans
  where
    allSpans = mainSpans ++ noteContents notesState
    (mainSpans, notesState) = runState (blocksToAutomergeSpans [] blocks) initialState
    initialState = NoteData 0 []

blocksToAutomergeSpans :: [Automerge.BlockType] -> [Pandoc.Block] -> NotesState [Automerge.Span]
blocksToAutomergeSpans parentBlockTypes blocks = concat <$> mapM (blockToAutomergeSpans parentBlockTypes) blocks

blockToAutomergeSpans :: [Automerge.BlockType] -> Pandoc.Block -> NotesState [Automerge.Span]
blockToAutomergeSpans parentBlockTypes block = case block of
  Pandoc.Plain inlines -> inlinesToAutomergeSpans parentBlockTypes inlines
  Pandoc.Para inlines -> do
    inlineSpans <- inlinesToAutomergeSpans parentBlockTypes inlines
    let blockSpan = Automerge.BlockSpan $ AutomergeBlock ParagraphMarker parentBlockTypes False
    return $ blockSpan : inlineSpans
  Pandoc.Header level _ inlines -> do
    inlineSpans <- inlinesToAutomergeSpans parentBlockTypes inlines
    let blockSpan = Automerge.BlockSpan $ AutomergeBlock (Automerge.HeadingMarker $ Heading $ HeadingLevel level) parentBlockTypes False
    return $ blockSpan : inlineSpans
  Pandoc.CodeBlock _ text ->
    return
      [ Automerge.BlockSpan $ AutomergeBlock Automerge.CodeBlockMarker parentBlockTypes False,
        Automerge.TextSpan $ AutomergeText text []
      ]
  Pandoc.BulletList items ->
    concat <$> mapM (containerBlockToSpans parentBlockTypes BulletListItem) items
  Pandoc.OrderedList _ items ->
    concat <$> mapM (containerBlockToSpans parentBlockTypes OrderedListItem) items
  Pandoc.BlockQuote blocks ->
    containerBlockToSpans parentBlockTypes PandocWriter.BlockQuote blocks
  _ -> return [] -- Ignore blocks we don't recognize

containerBlockToSpans :: [Automerge.BlockType] -> ContainerBlockType -> [Pandoc.Block] -> NotesState [Automerge.Span]
containerBlockToSpans parents itemType children = do
  let containerSpan = containerBlockToSpan parents itemType
  childSpans <- containerBlockChildrenToSpans parents itemType children
  return (containerSpan : childSpans)
  where
    containerBlockToSpan :: [Automerge.BlockType] -> ContainerBlockType -> Automerge.Span
    containerBlockToSpan parentBlockTypes BulletListItem = Automerge.BlockSpan $ AutomergeBlock Automerge.UnorderedListItemMarker parentBlockTypes False
    containerBlockToSpan parentBlockTypes OrderedListItem = Automerge.BlockSpan $ AutomergeBlock Automerge.OrderedListItemMarker parentBlockTypes False
    containerBlockToSpan parentBlockTypes PandocWriter.BlockQuote = Automerge.BlockSpan $ AutomergeBlock Automerge.BlockQuoteMarker parentBlockTypes False

containerBlockChildrenToSpans :: [Automerge.BlockType] -> ContainerBlockType -> [Pandoc.Block] -> NotesState [Automerge.Span]
containerBlockChildrenToSpans parentBlockTypes itemType blocks = concat <$> mapM (blockToAutomergeSpans (parentBlockTypes <> [toAutomergeBlockType itemType])) blocks

inlinesToAutomergeSpans :: [Automerge.BlockType] -> [Pandoc.Inline] -> NotesState [Automerge.Span]
inlinesToAutomergeSpans parents inlines = mergeSameMarkSpans <$> concat <$> mapM (inlineToAutomergeSpans parents) inlines

inlineToAutomergeSpans :: [Automerge.BlockType] -> Pandoc.Inline -> NotesState [Automerge.Span]
inlineToAutomergeSpans parents inline = case inline of
  Pandoc.Note noteBlocks -> do
    -- Generate note ID and create note content
    state <- get
    let newNoteId = noteCounter state + 1
        noteIdText = T.pack $ show newNoteId

    -- Convert note blocks to spans
    noteSpans <- blocksToAutomergeSpans parents noteBlocks
    let noteContentSpan = Automerge.BlockSpan $ AutomergeBlock (NoteContentMarker $ Automerge.NoteId noteIdText) [] False
        allNoteSpans = noteContentSpan : noteSpans

    -- Update state
    modify $ \s ->
      s
        { noteCounter = newNoteId,
          noteContents = noteContents s ++ allNoteSpans
        }

    -- Return embedded note reference span
    return [Automerge.BlockSpan $ AutomergeBlock (NoteRefMarker $ Automerge.NoteId noteIdText) [] True]
  Pandoc.Strong inlines -> do
    wrappedSpans <- inlinesToAutomergeSpans parents inlines
    return $ addMark Automerge.Strong wrappedSpans
  Pandoc.Emph inlines -> do
    wrappedSpans <- inlinesToAutomergeSpans parents inlines
    return $ addMark Automerge.Emphasis wrappedSpans
  Pandoc.Link _ inlines (linkUrl, linkTitle) -> do
    wrappedSpans <- inlinesToAutomergeSpans parents inlines
    return $ addMark (Automerge.LinkMark $ Automerge.Link {url = linkUrl, title = linkTitle}) wrappedSpans
  _ -> return $ Automerge.TextSpan <$> inlineToTextSpan inline

mergeSameMarkSpans :: [Automerge.Span] -> [Automerge.Span]
mergeSameMarkSpans = foldr mergeOrAppendAdjacent []
  where
    mergeOrAppendAdjacent :: Automerge.Span -> [Automerge.Span] -> [Automerge.Span]
    mergeOrAppendAdjacent x [] = [x]
    -- We only merge if the adjacent spans are **text** spans and they have the same marks.
    mergeOrAppendAdjacent (TextSpan xTextSpan) (TextSpan firstOrRestTextSpan : rest)
      | marks xTextSpan == marks firstOrRestTextSpan =
          TextSpan (xTextSpan <> firstOrRestTextSpan) : rest
    mergeOrAppendAdjacent x rest = x : rest

inlineToTextSpan :: Pandoc.Inline -> [Automerge.TextSpan]
inlineToTextSpan inline = case inline of
  Pandoc.Str str -> [AutomergeText str []]
  Pandoc.Space -> [AutomergeText (T.pack " ") []]
  Pandoc.Code _ text -> [AutomergeText text [Automerge.Code]]
  -- TODO: Handle other inline elements
  _ -> []

addMark :: Automerge.Mark -> [Automerge.Span] -> [Automerge.Span]
addMark mark spans = fmap (addMarkToSpan mark) spans
  where
    addMarkToSpan :: Automerge.Mark -> Automerge.Span -> Automerge.Span
    addMarkToSpan m (Automerge.TextSpan textSpan) = Automerge.TextSpan $ AutomergeText T.empty [m] <> textSpan
    -- Leave non-text spans (like note refs) unchanged
    addMarkToSpan _ otherSpan = otherSpan
