module PandocReader (toPandoc) where

import Automerge (AutomergeSpan (..), BlockMarker (..), BlockSpan (..), Heading (..), HeadingLevel (..), Link (..), Mark (..), TextSpan (..), isParent, takeUntilBlockSpan)
import Control.Monad.Except (throwError)
import Data.List (find, groupBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Tree (Tree (Node), drawTree, foldTree, unfoldForest)
import Debug.Trace
import Text.Pandoc (PandocError (PandocSyntaxMapError))
import Text.Pandoc.Builder as Pandoc
  ( Block (..),
    Blocks,
    Inline (Str),
    Inlines,
    ListNumberDelim (DefaultDelim),
    ListNumberStyle (DefaultStyle),
    Pandoc,
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
import Utils.Sequence (firstValue)

data BlockNode = PandocBlock Pandoc.Block | BulletListItem | OrderedListItem deriving (Show)

data DocNode = Root | BlockNode BlockNode | InlineNode Pandoc.Inlines deriving (Show)

traceTree :: Tree DocNode -> Tree DocNode
traceTree tree = Debug.Trace.trace (drawTree $ fmap show tree) tree

buildTree :: [AutomergeSpan] -> Maybe (Tree DocNode)
buildTree [] = Nothing
buildTree spans = fmap traceTree $ Just (groupListItems (buildRawTree spans))

groupListItems :: Tree DocNode -> Tree DocNode
groupListItems = foldTree addListNodes
  where
    addListNodes :: DocNode -> [Tree DocNode] -> Tree DocNode
    addListNodes node subtrees = case node of
      Root -> Node Root $ groupAdjacentListItems subtrees
      BlockNode _ -> Node node $ groupAdjacentListItems subtrees
      InlineNode _ -> Node node subtrees
      where
        groupAdjacentListItems :: [Tree DocNode] -> [Tree DocNode]
        groupAdjacentListItems = concat . map nestListItemGroupsUnderList . groupBy isAdjacentListItemNode
          where
            isAdjacentListItemNode :: Tree DocNode -> Tree DocNode -> Bool
            isAdjacentListItemNode (Node (BlockNode (BulletListItem)) _) (Node (BlockNode (BulletListItem)) _) = True
            isAdjacentListItemNode (Node (BlockNode (OrderedListItem)) _) (Node (BlockNode (OrderedListItem)) _) = True
            isAdjacentListItemNode _ _ = False

            nestListItemGroupsUnderList :: [Tree DocNode] -> [Tree DocNode]
            nestListItemGroupsUnderList group = case (find listItemInGroup group) of
              Nothing -> group
              Just item -> case item of
                -- add bullet list node
                (Node (BlockNode (BulletListItem)) _) -> [Node (BlockNode $ PandocBlock $ Pandoc.BulletList []) group]
                -- add ordered list node
                (Node (BlockNode (OrderedListItem)) _) -> [Node (BlockNode $ PandocBlock $ Pandoc.OrderedList (1, DefaultStyle, DefaultDelim) []) group]
                _ -> group
              where
                listItemInGroup :: Tree DocNode -> Bool
                listItemInGroup (Node (BlockNode (BulletListItem)) _) = True
                listItemInGroup (Node (BlockNode (OrderedListItem)) _) = True
                listItemInGroup _ = False

buildRawTree :: [AutomergeSpan] -> Tree DocNode
buildRawTree spans = Node Root $ unfoldForest buildDocNode $ getChildBlockSeeds Nothing spans

buildDocNode :: (AutomergeSpan, [AutomergeSpan]) -> (DocNode, [(AutomergeSpan, [AutomergeSpan])])
buildDocNode (currentSpan, remainingSpans) = case currentSpan of
  (BlockSpan blockSpan@(AutomergeBlock blockMarker _)) -> (BlockNode $ buildBlockNode blockMarker, getChildSeeds blockSpan remainingSpans)
  (TextSpan textSpan) -> (InlineNode $ convertTextSpan textSpan, [])

getChildSeeds :: BlockSpan -> [AutomergeSpan] -> [(AutomergeSpan, [AutomergeSpan])]
getChildSeeds blockSpan = (addChildlessSeed . takeUntilBlockSpan) <> (getChildBlockSeeds $ Just blockSpan)
  where
    addChildlessSeed = map (\x -> (x, []))

getChildBlockSeeds :: Maybe BlockSpan -> [AutomergeSpan] -> [(AutomergeSpan, [AutomergeSpan])]
getChildBlockSeeds blockSpan = addChildBlocks
  where
    addChildBlocks [] = []
    addChildBlocks (x : xs) = case x of
      BlockSpan currentSpan | isParent blockSpan currentSpan -> (BlockSpan currentSpan, xs) : addChildBlocks xs
      _ -> addChildBlocks xs

buildBlockNode :: BlockMarker -> BlockNode
buildBlockNode blockMarker = case blockMarker of
  ParagraphMarker -> PandocBlock $ Pandoc.Para []
  HeadingMarker (Heading (HeadingLevel level)) -> PandocBlock $ Pandoc.Header level nullAttr []
  CodeBlockMarker -> PandocBlock $ Pandoc.CodeBlock nullAttr T.empty
  UnorderedListItemMarker -> BulletListItem
  OrderedListItemMarker -> OrderedListItem
  _ -> undefined -- more blocks to be implemented

toPandoc :: (PandocMonad m) => [AutomergeSpan] -> m Pandoc.Pandoc
toPandoc spans = case convertSpansToBlocks spans of
  Left err -> throwError err
  Right blocks -> pure $ Pandoc.doc blocks
  where
    convertSpansToBlocks :: [AutomergeSpan] -> Either PandocError Pandoc.Blocks
    convertSpansToBlocks = fromMaybe (Right $ Pandoc.fromList []) . fmap treeToPandocBlocks . buildTree

treeToPandocBlocks :: Tree DocNode -> Either PandocError Pandoc.Blocks
treeToPandocBlocks tree = sequenceA (foldTree treeNodeToPandocBlock tree) >>= getBlockSeq

getBlockSeq :: [BlockOrInlines] -> Either PandocError Pandoc.Blocks
getBlockSeq = fmap Pandoc.fromList . traverse assertBlock

data BlockOrInlines = BlockElement Pandoc.Block | InlineElement Pandoc.Inlines

assertBlock :: BlockOrInlines -> Either PandocError Pandoc.Block
assertBlock (BlockElement block) = Right block
assertBlock (InlineElement _) = Left $ PandocSyntaxMapError $ T.pack "Error in mapping: found orphan inline node"

assertInlines :: BlockOrInlines -> Either PandocError Pandoc.Inlines
assertInlines (BlockElement _) = Left $ PandocSyntaxMapError $ T.pack "Error in mapping: found block node in inline node slot"
assertInlines (InlineElement inlines) = Right $ inlines

treeNodeToPandocBlock :: DocNode -> [[Either PandocError BlockOrInlines]] -> [Either PandocError BlockOrInlines]
treeNodeToPandocBlock node childrenNodes = case node of
  Root -> concat childrenNodes
  (BlockNode (PandocBlock (Pandoc.Para _))) -> case concatChildrenInlines childrenNodes of
    Left err -> [Left err]
    Right inlines -> [Right $ BlockElement $ Pandoc.Para $ Pandoc.toList inlines]
  (BlockNode (PandocBlock (Pandoc.Header level attr _))) -> case concatChildrenInlines childrenNodes of
    Left err -> [Left err]
    Right inlines -> [Right $ BlockElement $ Pandoc.Header level attr $ Pandoc.toList inlines]
  (BlockNode (PandocBlock (Pandoc.CodeBlock attr _))) -> case concatChildrenInlines childrenNodes of
    Left err -> [Left err]
    Right inlines -> case firstInline inlines of
      Just (Str text) -> [Right $ BlockElement $ Pandoc.CodeBlock attr text]
      _ -> [Left $ PandocSyntaxMapError $ T.pack "Error in mapping: Could not extract code block text"]
  (BlockNode (BulletListItem)) -> wrapInlinesToPlain $ concat childrenNodes
  (BlockNode (OrderedListItem)) -> wrapInlinesToPlain $ concat childrenNodes
  (BlockNode (PandocBlock (Pandoc.BulletList _))) -> case mapToChildBlocks childrenNodes of
    Left err -> [Left err]
    Right blocks -> [Right $ BlockElement $ Pandoc.BulletList blocks]
  (BlockNode (PandocBlock (Pandoc.OrderedList attrs _))) -> case mapToChildBlocks childrenNodes of
    Left err -> [Left err]
    Right blocks -> [Right $ BlockElement $ Pandoc.OrderedList attrs blocks]
  (InlineNode inlines) -> [Right $ InlineElement inlines]
  -- TODO: Remove when all block types are handled
  _ -> undefined
  where
    concatChildrenInlines :: [[Either PandocError BlockOrInlines]] -> Either PandocError Pandoc.Inlines
    concatChildrenInlines children = concatInlines $ map (>>= assertInlines) $ concat children
      where
        concatInlines :: [Either PandocError Pandoc.Inlines] -> Either PandocError Pandoc.Inlines
        concatInlines eitherInlines = fmap mconcat $ sequenceA eitherInlines

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

convertTextSpan :: TextSpan -> Pandoc.Inlines
convertTextSpan = convertMarksToInlines <*> convertTextToInlines

convertTextToInlines :: TextSpan -> Pandoc.Inlines
convertTextToInlines = Pandoc.str . value

convertMarksToInlines :: TextSpan -> Pandoc.Inlines -> Pandoc.Inlines
convertMarksToInlines textSpan inlines = foldl' (flip markToInlines) inlines $ marks textSpan

markToInlines :: Mark -> Pandoc.Inlines -> Pandoc.Inlines
markToInlines mark = case mark of
  Automerge.Strong -> Pandoc.strong
  Automerge.Emphasis -> Pandoc.emph
  Automerge.LinkMark automergeLink -> Pandoc.link (url automergeLink) (title automergeLink)
