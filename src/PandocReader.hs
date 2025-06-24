{-# LANGUAGE OverloadedStrings #-}

module PandocReader (toPandoc, readAutomerge) where

import Automerge (BlockMarker (..), BlockSpan (..), Heading (..), HeadingLevel (..), Link (..), Mark (..), Span (..), TextSpan (..), isParent, parseAutomergeSpansText, takeUntilBlockSpan, takeUntilNextSameBlockTypeSibling)
import Control.Monad ((>=>))
import Control.Monad.Except (throwError)
import Data.List (find, groupBy)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, toList)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Tree (Tree (Node), drawTree, foldTree, unfoldForest)
import Debug.Trace
import Text.Pandoc (PandocError (PandocParseError, PandocSyntaxMapError), ReaderOptions)
import Text.Pandoc.Builder as Pandoc
  ( Block (..),
    Blocks,
    Inline (Str),
    Inlines,
    ListNumberDelim (DefaultDelim),
    ListNumberStyle (DefaultStyle),
    Pandoc,
    code,
    doc,
    emph,
    fromList,
    link,
    nullAttr,
    str,
    strong,
    toList,
  )
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Sources (ToSources, sourcesToText, toSources)
import Utils.Sequence (firstValue)

-- Althout ReaderOptions are not used, the function is written like this so that it's consistent with the other Pandoc reader functions.
readAutomerge :: (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc
readAutomerge _ =
  -- Using Kleisli composition to compose the 2 smaller functions in the monadic context (PandocMonad)
  parseSpansAndHandleErrors >=> toPandoc
  where
    -- Here we parse the JSON Automerge spans but also convert a potential error to a Pandoc parsing error
    -- The result is a list of Automerge spans wrapped within a Pandoc monad.
    parseSpansAndHandleErrors :: (ToSources a, PandocMonad m) => a -> m [Automerge.Span]
    parseSpansAndHandleErrors = either handleParsingErrorMessage pure . parseSpans

    parseSpans :: (ToSources a) => a -> Either String [Span]
    parseSpans = parseAutomergeSpansText . sourcesToText . toSources

    handleParsingErrorMessage :: (PandocMonad m) => String -> m a
    handleParsingErrorMessage = throwError . PandocParseError . T.pack

toPandoc :: (PandocMonad m) => [Automerge.Span] -> m Pandoc.Pandoc
toPandoc = (either throwError (pure . Pandoc.doc)) . convertSpansToBlocks
  where
    convertSpansToBlocks :: [Automerge.Span] -> Either PandocError Pandoc.Blocks
    convertSpansToBlocks = fromMaybe (Right $ Pandoc.fromList []) . fmap treeToPandocBlocks . buildTree

data BlockNode = PandocBlock Pandoc.Block | BulletListItem | OrderedListItem deriving (Show)

data DocNode = Root | BlockNode BlockNode | InlineNode Pandoc.Inlines deriving (Show)

traceTree :: Tree DocNode -> Tree DocNode
traceTree tree = Debug.Trace.trace (drawTree $ fmap show tree) tree

buildTree :: [Automerge.Span] -> Maybe (Tree DocNode)
buildTree = (fmap (traceTree . groupListItems . buildRawTree)) . nonEmpty

buildRawTree :: NonEmpty Automerge.Span -> Tree DocNode
buildRawTree spans = Node Root $ unfoldForest buildDocNode $ getChildBlockSeeds Nothing $ Data.List.NonEmpty.toList spans

buildDocNode :: (Automerge.Span, [Automerge.Span]) -> (DocNode, [(Automerge.Span, [Automerge.Span])])
buildDocNode (currentSpan, remainingSpans) = case currentSpan of
  (Automerge.BlockSpan blockSpan@(AutomergeBlock blockMarker _)) -> (BlockNode $ buildBlockNode blockMarker, getChildSeeds blockSpan remainingSpans)
  (Automerge.TextSpan textSpan) -> (InlineNode $ convertTextSpan textSpan, [])

getChildSeeds :: Automerge.BlockSpan -> [Automerge.Span] -> [(Automerge.Span, [Automerge.Span])]
getChildSeeds blockSpan = (addChildlessSeed . Automerge.takeUntilBlockSpan) <> (getChildBlockSeeds $ Just blockSpan)
  where
    addChildlessSeed = map (\x -> (x, []))

getChildBlockSeeds :: Maybe Automerge.BlockSpan -> [Automerge.Span] -> [(Automerge.Span, [Automerge.Span])]
getChildBlockSeeds blockSpan = addChildBlocks
  where
    addChildBlocks [] = []
    addChildBlocks (x : xs) = case x of
      Automerge.BlockSpan currentSpan | Automerge.isParent blockSpan currentSpan -> createChildBlockSeed currentSpan xs : addChildBlocks xs
      _ -> addChildBlocks xs
      where
        createChildBlockSeed :: BlockSpan -> [Automerge.Span] -> (Automerge.Span, [Automerge.Span])
        -- We stop seeding when we encounter a same block type sibling so that children don't get added (replicated) to all siblings
        createChildBlockSeed blSpan restSpans = (Automerge.BlockSpan blSpan, Automerge.takeUntilNextSameBlockTypeSibling blSpan restSpans)

buildBlockNode :: BlockMarker -> BlockNode
buildBlockNode blockMarker = case blockMarker of
  Automerge.ParagraphMarker -> PandocBlock $ Pandoc.Para []
  Automerge.HeadingMarker (Heading (HeadingLevel level)) -> PandocBlock $ Pandoc.Header level nullAttr []
  Automerge.CodeBlockMarker -> PandocBlock $ Pandoc.CodeBlock nullAttr T.empty
  Automerge.UnorderedListItemMarker -> BulletListItem
  Automerge.OrderedListItemMarker -> OrderedListItem
  _ -> undefined -- more blocks to be implemented

convertTextSpan :: Automerge.TextSpan -> Pandoc.Inlines
convertTextSpan = convertMarksToInlines <*> convertTextToInlines

convertTextToInlines :: Automerge.TextSpan -> Pandoc.Inlines
convertTextToInlines = Pandoc.str . value

convertMarksToInlines :: Automerge.TextSpan -> Pandoc.Inlines -> Pandoc.Inlines
convertMarksToInlines textSpan inlines = foldl' (flip markToInlines) inlines $ marks textSpan

markToInlines :: Automerge.Mark -> Pandoc.Inlines -> Pandoc.Inlines
markToInlines mark = case mark of
  Automerge.Strong -> Pandoc.strong
  Automerge.Emphasis -> Pandoc.emph
  Automerge.LinkMark automergeLink -> Pandoc.link (url automergeLink) (title automergeLink)
  Automerge.Code -> Pandoc.code . concatStrInlines
    where
      concatStrInlines :: Inlines -> T.Text
      concatStrInlines inlines = T.concat [t | Pandoc.Str t <- Pandoc.toList inlines]

groupListItems :: Tree DocNode -> Tree DocNode
groupListItems = foldTree addListNodes
  where
    addListNodes :: DocNode -> [Tree DocNode] -> Tree DocNode
    addListNodes node subtrees = Node node $ case node of
      Root -> groupAdjacentListItems subtrees
      BlockNode _ -> groupAdjacentListItems subtrees
      InlineNode _ -> subtrees

groupAdjacentListItems :: [Tree DocNode] -> [Tree DocNode]
groupAdjacentListItems = concatMap nestListItemGroupsUnderList . groupBy isAdjacentListItemNode
  where
    isAdjacentListItemNode :: Tree DocNode -> Tree DocNode -> Bool
    isAdjacentListItemNode (Node (BlockNode (BulletListItem)) _) (Node (BlockNode (BulletListItem)) _) = True
    isAdjacentListItemNode (Node (BlockNode (OrderedListItem)) _) (Node (BlockNode (OrderedListItem)) _) = True
    isAdjacentListItemNode _ _ = False

    nestListItemGroupsUnderList :: [Tree DocNode] -> [Tree DocNode]
    nestListItemGroupsUnderList group = case (find listItemInGroup group) of
      -- add bullet list node
      Just (Node (BlockNode (BulletListItem)) _) -> [Node (BlockNode $ PandocBlock $ Pandoc.BulletList []) group]
      -- add ordered list node
      Just (Node (BlockNode (OrderedListItem)) _) -> [Node (BlockNode $ PandocBlock $ Pandoc.OrderedList (1, DefaultStyle, DefaultDelim) []) group]
      _ -> group

listItemInGroup :: Tree DocNode -> Bool
listItemInGroup (Node (BlockNode (BulletListItem)) _) = True
listItemInGroup (Node (BlockNode (OrderedListItem)) _) = True
listItemInGroup _ = False

treeToPandocBlocks :: Tree DocNode -> Either PandocError Pandoc.Blocks
treeToPandocBlocks tree = sequenceA (foldTree treeNodeToPandocBlock tree) >>= getBlockSeq

data BlockOrInlines = BlockElement Pandoc.Block | InlineElement Pandoc.Inlines

treeNodeToPandocBlock :: DocNode -> [[Either PandocError BlockOrInlines]] -> [Either PandocError BlockOrInlines]
treeNodeToPandocBlock node childrenNodes = case node of
  Root -> concat childrenNodes
  (BlockNode (PandocBlock (Pandoc.Para _))) -> [fmap (BlockElement . Pandoc.Para . Pandoc.toList) (concatChildrenInlines childrenNodes)]
  (BlockNode (PandocBlock (Pandoc.Header level attr _))) -> [fmap (BlockElement . Pandoc.Header level attr . Pandoc.toList) (concatChildrenInlines childrenNodes)]
  (BlockNode (PandocBlock (Pandoc.CodeBlock attr _))) ->
    [ do
        inlines <- concatChildrenInlines childrenNodes
        case firstInline inlines of
          Just (Str text) -> Right $ BlockElement $ Pandoc.CodeBlock attr text
          _ -> Left $ PandocSyntaxMapError "Error in mapping: Could not extract code block text"
    ]
  (BlockNode (BulletListItem)) -> wrapInlinesToPlain . concatAdjacentInlines $ concat childrenNodes
  (BlockNode (OrderedListItem)) -> wrapInlinesToPlain . concatAdjacentInlines $ concat childrenNodes
  (BlockNode (PandocBlock (Pandoc.BulletList _))) -> [fmap (BlockElement . Pandoc.BulletList) (mapToChildBlocks childrenNodes)]
  (BlockNode (PandocBlock (Pandoc.OrderedList attrs _))) -> [fmap (BlockElement . Pandoc.OrderedList attrs) (mapToChildBlocks childrenNodes)]
  (InlineNode inlines) -> [Right $ InlineElement inlines]
  -- TODO: Remove when all block types are handled
  _ -> undefined
  where
    concatChildrenInlines :: [[Either PandocError BlockOrInlines]] -> Either PandocError Pandoc.Inlines
    concatChildrenInlines children = concatInlines $ map (>>= assertInlines) $ concat children
      where
        concatInlines :: [Either PandocError Pandoc.Inlines] -> Either PandocError Pandoc.Inlines
        concatInlines eitherInlines = fmap mconcat $ sequenceA eitherInlines

    concatAdjacentInlines :: [Either PandocError BlockOrInlines] -> [Either PandocError BlockOrInlines]
    concatAdjacentInlines = foldr mergeOrAppendAdjacent []
      where
        mergeOrAppendAdjacent :: Either PandocError BlockOrInlines -> [Either PandocError BlockOrInlines] -> [Either PandocError BlockOrInlines]
        mergeOrAppendAdjacent x [] = [x]
        mergeOrAppendAdjacent (Right (InlineElement currentInlines)) (Right (InlineElement firstOfRestInlines) : rest) =
          (Right (InlineElement (currentInlines <> firstOfRestInlines)) : rest)
        mergeOrAppendAdjacent x rest = (x : rest)

    wrapInlinesToPlain :: [Either PandocError BlockOrInlines] -> [Either PandocError BlockOrInlines]
    wrapInlinesToPlain eitherInlines = (fmap . fmap) wrapInlines eitherInlines
      where
        wrapInlines :: BlockOrInlines -> BlockOrInlines
        wrapInlines (BlockElement block) = BlockElement block
        wrapInlines (InlineElement inlines) = BlockElement $ Pandoc.Plain $ Pandoc.toList inlines

    mapToChildBlocks :: [[Either PandocError BlockOrInlines]] -> Either PandocError [[Pandoc.Block]]
    mapToChildBlocks children = (traverse . traverse) (>>= assertBlock) children

    firstInline :: Pandoc.Inlines -> Maybe Pandoc.Inline
    firstInline = firstValue

getBlockSeq :: [BlockOrInlines] -> Either PandocError Pandoc.Blocks
getBlockSeq = fmap Pandoc.fromList . traverse assertBlock

assertBlock :: BlockOrInlines -> Either PandocError Pandoc.Block
assertBlock (BlockElement block) = Right block
assertBlock (InlineElement _) = Left $ PandocSyntaxMapError "Error in mapping: found orphan inline node"

assertInlines :: BlockOrInlines -> Either PandocError Pandoc.Inlines
assertInlines (BlockElement _) = Left $ PandocSyntaxMapError "Error in mapping: found block node in inline node slot"
assertInlines (InlineElement inlines) = Right $ inlines