module PandocReader (toPandoc) where

import Automerge (AutomergeSpan (..), BlockMarker (..), BlockSpan (..), Heading (..), HeadingLevel (..), Link (..), Mark (..), TextSpan (..), isParent, isSiblingListItem, isTopLevelBlock, takeUntilBlockSpan)
import Data.Sequence as Seq (Seq (Empty))
import qualified Data.Text as T
import Data.Tree (Tree (Node), unfoldForest)
import Text.Pandoc.Builder (Blocks, Inlines, Many (..), blockQuote, codeBlockWith, doc, emph, fromList, headerWith, link, para, str, strong)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Utils.Sequence (lastValue, withoutLast)

data DocNode = Root | BlockNode Block | InlineNode Inlines

buildTree :: [AutomergeSpan] -> Maybe (Tree DocNode)
buildTree [] = Nothing
-- TODO: This is probably wrong, we probably want to start with something like Root, not the first item.
buildTree spans = Just $ Node Root $ unfoldForest buildDocNode $ getRootSeeds spans

getRootSeeds :: [AutomergeSpan] -> [(AutomergeSpan, [AutomergeSpan])]
getRootSeeds [] = []
getRootSeeds (x : xs) = case x of
  BlockSpan currentSpan | (isTopLevelBlock currentSpan) -> (BlockSpan currentSpan, xs) : getRootSeeds xs
  _ -> getRootSeeds xs

buildDocNode :: (AutomergeSpan, [AutomergeSpan]) -> (DocNode, [(AutomergeSpan, [AutomergeSpan])])
buildDocNode (currentSpan, remainingSpans) = case currentSpan of
  (BlockSpan blockSpan@(AutomergeBlock blockMarker _)) -> (BlockNode $ convertBlockMarker blockMarker, getChildSeeds blockSpan remainingSpans)
  (TextSpan textSpan) -> (InlineNode $ convertTextSpan textSpan, [])

getChildSeeds :: BlockSpan -> [AutomergeSpan] -> [(AutomergeSpan, [AutomergeSpan])]
getChildSeeds blockSpan = (addChildlessSeed . takeUntilBlockSpan) <> (findChildBlocksWithRemainder blockSpan)
  where
    addChildlessSeed = map (\x -> (x, []))

findChildBlocksWithRemainder :: BlockSpan -> [AutomergeSpan] -> [(AutomergeSpan, [AutomergeSpan])]
findChildBlocksWithRemainder blockSpan = addChildBlocks
  where
    addChildBlocks [] = []
    addChildBlocks (x : xs) = case x of
      -- If a child span is encountered, add it to the list (along with the remainder)
      -- using the cons operator and continue with the recursion
      BlockSpan currentSpan | (isParent blockSpan currentSpan || isSiblingListItem blockSpan currentSpan) -> (BlockSpan currentSpan, xs) : addChildBlocks xs
      _ -> addChildBlocks xs

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
