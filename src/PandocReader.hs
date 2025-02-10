module PandocReader (toPandoc) where

import Automerge (AutomergeSpan (..), BlockMarker (..), BlockSpan (..), Heading (..), HeadingLevel (..), Link (..), Mark (..), TextSpan (..), isParent, takeUntilBlockSpan)
import Data.List (find, groupBy)
import Data.Sequence as Seq (Seq (Empty))
import qualified Data.Text as T
import Data.Tree (Tree (Node), foldTree, unfoldForest)
import Text.Pandoc.Builder (Blocks, Inlines, Many (..), blockQuote, codeBlockWith, doc, emph, fromList, headerWith, link, para, str, strong)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Utils.Sequence (lastValue, withoutLast)

data ListItem = Item [Block]

data BlockNode = PandocBlock Block | BulletListItem ListItem | OrderedListItem ListItem

data DocNode = Root | BlockNode BlockNode | InlineNode Inlines

buildTree :: [AutomergeSpan] -> Maybe (Tree DocNode)
buildTree [] = Nothing
buildTree spans = Just $ groupListItems $ buildRawTree spans

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
            isAdjacentListItemNode (Node (BlockNode (BulletListItem _)) _) (Node (BlockNode (BulletListItem _)) _) = True
            isAdjacentListItemNode (Node (BlockNode (OrderedListItem _)) _) (Node (BlockNode (OrderedListItem _)) _) = True
            isAdjacentListItemNode _ _ = False

            nestListItemGroupsUnderList :: [Tree DocNode] -> [Tree DocNode]
            nestListItemGroupsUnderList group = case (find listItemInGroup group) of
              Nothing -> group
              Just item -> case item of
                -- add bullet list node
                (Node (BlockNode (BulletListItem _)) _) -> [Node (BlockNode $ PandocBlock $ BulletList []) group]
                -- add ordered list node
                (Node (BlockNode (OrderedListItem _)) _) -> [Node (BlockNode $ PandocBlock $ OrderedList (1, DefaultStyle, DefaultDelim) []) group]
                _ -> group
              where
                listItemInGroup :: Tree DocNode -> Bool
                listItemInGroup (Node (BlockNode (BulletListItem _)) _) = True
                listItemInGroup (Node (BlockNode (OrderedListItem _)) _) = True
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
  ParagraphMarker -> PandocBlock $ Para []
  HeadingMarker (Heading (HeadingLevel level)) -> PandocBlock $ Header level nullAttr []
  CodeBlockMarker -> PandocBlock $ CodeBlock nullAttr T.empty
  UnorderedListItemMarker -> BulletListItem $ Item []
  OrderedListItemMarker -> OrderedListItem $ Item []
  _ -> undefined -- more blocks to be implemented

toPandoc :: (PandocMonad m) => [AutomergeSpan] -> m Pandoc
toPandoc spans = pure . doc $ convertAutomergeSpans spans

convertAutomergeSpans :: [AutomergeSpan] -> Blocks
convertAutomergeSpans = foldl' convertAutomergeSpan (Many Seq.Empty)

convertAutomergeSpan :: Blocks -> AutomergeSpan -> Blocks
convertAutomergeSpan acc (BlockSpan (AutomergeBlock blockMarker _)) = acc <> (fromList [convertBlockMarker blockMarker])
convertAutomergeSpan acc (TextSpan textSpan) = case lastValue acc of
  Nothing -> acc <> convertAndWrapToParagraph textSpan
  Just block -> withoutLast acc <> convertAndAddTo block textSpan

convertBlockMarker :: BlockMarker -> Block
convertBlockMarker blockSpan = case blockSpan of
  ParagraphMarker -> Para []
  HeadingMarker (Heading (HeadingLevel level)) -> Header level nullAttr []
  CodeBlockMarker -> CodeBlock nullAttr T.empty
  _ -> undefined -- more blocks to be implemented

convertAndWrapToParagraph :: TextSpan -> Blocks
convertAndWrapToParagraph = para . convertTextSpan

convertAndAddTo :: Block -> TextSpan -> Blocks
convertAndAddTo block textSpan = case block of
  Para inlines -> para $ addTextSpanToInlines (fromList inlines) textSpan
  Header level attr inlines -> headerWith attr level $ addTextSpanToInlines (fromList inlines) textSpan
  CodeBlock attr _ -> codeBlockWith attr $ value textSpan
  BlockQuote _ -> blockQuote $ convertAndWrapToParagraph textSpan
  _ -> undefined

addTextSpanToInlines :: Inlines -> TextSpan -> Inlines
addTextSpanToInlines inlines textSpan = inlines <> convertTextSpan textSpan

convertTextSpan :: TextSpan -> Inlines
convertTextSpan = convertMarksToInlines <*> convertTextToInlines

convertTextToInlines :: TextSpan -> Inlines
convertTextToInlines = str . value

convertMarksToInlines :: TextSpan -> Inlines -> Inlines
convertMarksToInlines textSpan inlines = foldl' (flip markToInlines) inlines $ marks textSpan

markToInlines :: Mark -> Inlines -> Inlines
markToInlines mark = case mark of
  Automerge.Strong -> strong
  Automerge.Emphasis -> emph
  Automerge.LinkMark automergeLink -> link (url automergeLink) (title automergeLink)
