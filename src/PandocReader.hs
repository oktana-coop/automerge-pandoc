module PandocReader where

import Automerge (AutomergeSpan (..), BlockMarker (..), Heading (..), HeadingLevel (..), Mark (..), TextSpan (..))
import Data.Foldable (foldl') -- Import foldl' from Data.Foldable
import Data.Sequence as Seq (Seq (Empty))
import qualified Data.Text as T
import Text.Pandoc.Builder (Blocks, Inlines, Many (..), blockQuote, codeBlock, codeBlockWith, doc, emph, header, headerWith, para, str, strong)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Utils (lastValue)

toPandoc :: (PandocMonad m) => [AutomergeSpan] -> m Pandoc
toPandoc spans = pure . doc $ convertAutomergeSpans spans

convertAutomergeSpans :: [AutomergeSpan] -> Blocks
convertAutomergeSpans = foldl' convertAutomergeSpan (Many Seq.Empty)

convertAutomergeSpan :: Blocks -> AutomergeSpan -> Blocks
convertAutomergeSpan acc (BlockSpan blockSpan) = acc <> convertBlockSpan blockSpan
convertAutomergeSpan acc (TextSpan textSpan) = acc <> convertAndWrapTextSpan (lastValue acc) textSpan

convertBlockSpan :: BlockMarker -> Blocks
convertBlockSpan blockSpan = case blockSpan of
  ParagraphMarker -> para (Many Seq.Empty)
  HeadingMarker (Heading (HeadingLevel level)) -> header level $ Many Seq.Empty
  CodeBlockMarker -> codeBlock T.empty
  BlockQuoteMarker -> blockQuote $ Many Seq.Empty
  _ -> undefined -- more blocks to be implemented

convertAndWrapTextSpan :: Maybe Block -> TextSpan -> Blocks
convertAndWrapTextSpan Nothing textSpan = para $ convertTextSpan textSpan
convertAndWrapTextSpan (Just block) textSpan = case block of
  Para _ -> para $ convertTextSpan textSpan
  Header level attr _ -> headerWith attr level $ convertTextSpan textSpan
  CodeBlock attr _ -> codeBlockWith attr $ value textSpan
  BlockQuote _ -> blockQuote $ para $ convertTextSpan textSpan
  _ -> undefined

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
  _ -> undefined -- TODO: Handle link