module PandocReader where

import Automerge (AutomergeSpan (..), BlockMarker (..), Heading (..), HeadingLevel (..), TextSpan (..))
import Data.Foldable (foldl') -- Import foldl' from Data.Foldable
import Data.Sequence as Seq (Seq (Empty))
import qualified Data.Text as T
import Text.Pandoc.Builder (Blocks, Many (..), blockQuote, codeBlock, doc, header, para, str)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Utils (lastValue)

toPandoc :: (PandocMonad m) => [AutomergeSpan] -> m Pandoc
toPandoc spans = pure . doc $ convertAutomergeSpans spans

convertAutomergeSpans :: [AutomergeSpan] -> Blocks
convertAutomergeSpans = foldl' convertAutomergeSpan (Many Seq.Empty)

convertAutomergeSpan :: Blocks -> AutomergeSpan -> Blocks
convertAutomergeSpan acc (BlockSpan blockSpan) = acc <> convertBlockSpan blockSpan
convertAutomergeSpan acc (TextSpan textSpan) = acc <> convertTextSpan (lastValue acc) textSpan

convertBlockSpan :: BlockMarker -> Blocks
convertBlockSpan blockSpan = case blockSpan of
  ParagraphMarker -> para (Many Seq.Empty)
  HeadingMarker (Heading (HeadingLevel level)) -> header level $ Many Seq.Empty
  CodeBlockMarker -> codeBlock T.empty
  BlockQuoteMarker -> blockQuote $ Many Seq.Empty
  _ -> undefined -- more blocks to be implemented

convertTextSpan :: Maybe Block -> TextSpan -> Blocks
convertTextSpan Nothing textSpan = para $ str $ value textSpan -- TODO: handle marks
convertTextSpan (Just block) textSpan = undefined