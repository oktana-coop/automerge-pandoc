module PandocWriter (writeAutomerge) where

import Automerge (BlockMarker (..), BlockSpan (..), BlockType (..), CodeBlock (..), CodeBlockLanguage (..), Heading (..), HeadingLevel (..), Image (..), Link (..), Mark (..), NoteId (..), Span (..), TextSpan (..), toJSONText)
import Control.Monad.State (State, get, modify, runState)
import qualified Data.Text as T
import Text.Pandoc (WriterOptions)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition as Pandoc (Attr, Block (..), Caption (..), Inline (..), Pandoc (Pandoc))

data ContainerBlockType = BulletListItem | OrderedListItem | BlockQuote | Figure | Caption deriving (Show, Eq)

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
toAutomergeBlockType PandocWriter.Figure = Automerge.FigureType
toAutomergeBlockType PandocWriter.Caption = Automerge.CaptionType

writeAutomerge :: (PandocMonad m) => WriterOptions -> Pandoc.Pandoc -> m T.Text
writeAutomerge _ (Pandoc.Pandoc _ blocks) = pure $ toJSONText automergeSpans
  where
    automergeSpans = mainSpans ++ noteContents notesState
    (mainSpans, notesState) = runState (blocksToAutomergeSpans [] blocks) initialState
    initialState = NoteData 0 []

blocksToAutomergeSpans :: [Automerge.BlockType] -> [Pandoc.Block] -> NotesState [Automerge.Span]
blocksToAutomergeSpans parentBlockTypes blocks = fmap concat (perBlockSpans blocks)
  where
    -- Convert each block into a list of spans (per block), inside the State monad.
    perBlockSpans :: [Pandoc.Block] -> NotesState [[Automerge.Span]]
    perBlockSpans = mapM (blockToAutomergeSpans parentBlockTypes)

blockToAutomergeSpans :: [Automerge.BlockType] -> Pandoc.Block -> NotesState [Automerge.Span]
blockToAutomergeSpans parentBlockTypes block = case block of
  -- Plain has no Automerge marker — its inlines flow into the preceding non-embed block's span sequence.
  -- Embed inlines (Image, NoteRef) inside a Plain with a non-embed sibling will be attributed to that sibling.
  -- TODO: revisit — likely needs a Plain marker in the model or a span-level barrier to preserve the wrapper.
  Pandoc.Plain inlines -> inlinesToAutomergeSpans parentBlockTypes inlines
  Pandoc.Para inlines -> do
    inlineSpans <- inlinesToAutomergeSpans (parentBlockTypes <> [ParagraphType]) inlines
    let blockSpan = Automerge.BlockSpan $ AutomergeBlock ParagraphMarker parentBlockTypes False
    return (blockSpan : inlineSpans)
  Pandoc.Header level _ inlines -> do
    inlineSpans <- inlinesToAutomergeSpans (parentBlockTypes <> [HeadingType]) inlines
    let blockSpan = Automerge.BlockSpan $ AutomergeBlock (Automerge.HeadingMarker $ Heading $ HeadingLevel level) parentBlockTypes False
    return (blockSpan : inlineSpans)
  Pandoc.CodeBlock attr text ->
    return
      [ Automerge.BlockSpan $ AutomergeBlock (Automerge.CodeBlockMarker $ Automerge.CodeBlock $ codeBlockLanguageFromPandocAttr attr) parentBlockTypes False,
        Automerge.TextSpan $ AutomergeText text []
      ]
  Pandoc.BulletList items ->
    concat <$> mapM (containerBlockToAutomergeSpans parentBlockTypes BulletListItem) items
  Pandoc.OrderedList _ items ->
    concat <$> mapM (containerBlockToAutomergeSpans parentBlockTypes OrderedListItem) items
  Pandoc.BlockQuote blocks ->
    containerBlockToAutomergeSpans parentBlockTypes PandocWriter.BlockQuote blocks
  Pandoc.HorizontalRule -> return [Automerge.BlockSpan $ AutomergeBlock HorizontalRuleMarker parentBlockTypes False]
  Pandoc.Figure _ figCaption figBlocks -> do
    let figureSpan = containerBlockToSpan parentBlockTypes PandocWriter.Figure
        figureChildParents = parentBlockTypes <> [toAutomergeBlockType PandocWriter.Figure]
    -- Emit figure blocks before the caption so embed blocks (e.g. ImageMarker) attach to the figure, not the caption.
    blockSpans <- blocksToAutomergeSpans figureChildParents figBlocks
    captionSpans <- captionToAutomergeSpans figureChildParents figCaption
    return (figureSpan : blockSpans ++ captionSpans)
  _ -> return [] -- Ignore blocks we don't recognize

captionToAutomergeSpans :: [Automerge.BlockType] -> Pandoc.Caption -> NotesState [Automerge.Span]
captionToAutomergeSpans _ (Pandoc.Caption Nothing []) = return []
captionToAutomergeSpans captionParents (Pandoc.Caption maybeShortCap capBlocks) = do
  let captionSpan = containerBlockToSpan captionParents PandocWriter.Caption
      childParents = captionParents <> [toAutomergeBlockType PandocWriter.Caption]
  shortCapSpans <- maybe (return []) (inlinesToAutomergeSpans childParents) maybeShortCap
  blockSpans <- blocksToAutomergeSpans childParents capBlocks
  return (captionSpan : shortCapSpans ++ blockSpans)

codeBlockLanguageFromPandocAttr :: Pandoc.Attr -> Maybe CodeBlockLanguage
codeBlockLanguageFromPandocAttr (_, classes, _) = case classes of
  [] -> Nothing
  -- Assuming language is the first class in Pandoc class attributes.
  language : _ -> Just $ Automerge.CodeBlockLanguage language

containerBlockToAutomergeSpans :: [Automerge.BlockType] -> ContainerBlockType -> [Pandoc.Block] -> NotesState [Automerge.Span]
containerBlockToAutomergeSpans parents itemType children = do
  let containerSpan = containerBlockToSpan parents itemType
      childParentTypes = parents <> [toAutomergeBlockType itemType]
  childSpans <- fmap concat $ mapM (blockToAutomergeSpans childParentTypes) children
  return (containerSpan : childSpans)

containerBlockToSpan :: [Automerge.BlockType] -> ContainerBlockType -> Automerge.Span
containerBlockToSpan parents containerType =
  Automerge.BlockSpan $ AutomergeBlock (containerBlockMarker containerType) parents False
  where
    containerBlockMarker :: ContainerBlockType -> BlockMarker
    containerBlockMarker BulletListItem = UnorderedListItemMarker
    containerBlockMarker OrderedListItem = OrderedListItemMarker
    containerBlockMarker PandocWriter.BlockQuote = BlockQuoteMarker
    containerBlockMarker PandocWriter.Figure = FigureMarker
    containerBlockMarker PandocWriter.Caption = CaptionMarker

inlinesToAutomergeSpans :: [Automerge.BlockType] -> [Pandoc.Inline] -> NotesState [Automerge.Span]
inlinesToAutomergeSpans parents inlines =
  -- Use fmap to lift `mergeSameMarkSpans . concat` over the State structure
  fmap (mergeSameMarkSpans . concat) (perInlineSpans inlines)
  where
    -- Convert each inline into a list of spans, inside the State monad.
    perInlineSpans :: [Pandoc.Inline] -> NotesState [[Automerge.Span]]
    perInlineSpans = mapM (inlineToAutomergeSpans parents)

inlineToAutomergeSpans :: [Automerge.BlockType] -> Pandoc.Inline -> NotesState [Automerge.Span]
inlineToAutomergeSpans parents inline = case inline of
  Pandoc.Note noteBlocks -> do
    -- Generate note ID and create note content
    notesState <- get
    let newNoteId = noteCounter notesState + 1
        noteIdText = T.pack $ show newNoteId

    -- Convert note blocks to spans
    noteContentChildBlockSpans <- blocksToAutomergeSpans [NoteContentType] noteBlocks
    let noteContentSpan = Automerge.BlockSpan $ AutomergeBlock (NoteContentMarker $ Automerge.NoteId noteIdText) [] False
        noteContentSpans = noteContentSpan : noteContentChildBlockSpans

    -- Update state
    modify
      ( \currentNotestState ->
          -- Getting a new state using the record update syntax.
          currentNotestState
            { noteCounter = newNoteId,
              noteContents = noteContents currentNotestState ++ noteContentSpans
            }
      )

    -- Return embedded note reference span
    return [Automerge.BlockSpan $ AutomergeBlock (NoteRefMarker $ Automerge.NoteId noteIdText) parents True]
  Pandoc.Strong inlines -> do
    wrappedSpans <- inlinesToAutomergeSpans parents inlines
    return $ addMark Automerge.Strong wrappedSpans
  Pandoc.Emph inlines -> do
    wrappedSpans <- inlinesToAutomergeSpans parents inlines
    return $ addMark Automerge.Emphasis wrappedSpans
  Pandoc.Link _ inlines (linkUrl, linkTtl) -> do
    wrappedSpans <- inlinesToAutomergeSpans parents inlines
    return $ addMark (Automerge.LinkMark $ Automerge.Link {url = linkUrl, linkTitle = linkTtl}) wrappedSpans
  Pandoc.Image _ altInlines (imgUrl, imgTtl) ->
    return [Automerge.BlockSpan $ AutomergeBlock (ImageMarker $ buildAutomergeImage imgUrl imgTtl altInlines) parents True]
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

extractTextFromInline :: Pandoc.Inline -> T.Text
extractTextFromInline (Pandoc.Str s) = s
extractTextFromInline Pandoc.Space = T.pack " "
extractTextFromInline Pandoc.SoftBreak = T.pack " "
extractTextFromInline Pandoc.LineBreak = T.pack "\n"
extractTextFromInline (Pandoc.Code _ txt) = txt
extractTextFromInline _ = T.empty

inlineToTextSpan :: Pandoc.Inline -> [Automerge.TextSpan]
inlineToTextSpan inline = case inline of
  Pandoc.Str _ -> [AutomergeText (extractTextFromInline inline) []]
  Pandoc.Space -> [AutomergeText (extractTextFromInline inline) []]
  Pandoc.SoftBreak -> [AutomergeText (extractTextFromInline inline) []]
  Pandoc.LineBreak -> [AutomergeText (extractTextFromInline inline) []]
  Pandoc.Code _ txt -> [AutomergeText txt [Automerge.Code]]
  -- TODO: Handle other inline elements
  _ -> []

buildAutomergeImage :: T.Text -> T.Text -> [Pandoc.Inline] -> Automerge.Image
buildAutomergeImage imgUrl imgTtl altInlines =
  Automerge.Image imgUrl (textToMaybe imgTtl) (textToMaybe (extractTextFromInlines altInlines))
  where
    textToMaybe t = if T.null t then Nothing else Just t

extractTextFromInlines :: [Pandoc.Inline] -> T.Text
extractTextFromInlines = foldMap extractTextFromInline

addMark :: Automerge.Mark -> [Automerge.Span] -> [Automerge.Span]
addMark mark spans = fmap (addMarkToSpan mark) spans
  where
    addMarkToSpan :: Automerge.Mark -> Automerge.Span -> Automerge.Span
    addMarkToSpan m (Automerge.TextSpan textSpan) = Automerge.TextSpan $ AutomergeText T.empty [m] <> textSpan
    -- Leave non-text spans (like note refs) unchanged
    addMarkToSpan _ otherSpan = otherSpan
