module PandocReader where

import Automerge (AutomergeSpan (..), BlockMarker (..), Heading (..), HeadingLevel (..))
import Data.Foldable (foldl', toList) -- Import foldl' from Data.Foldable
import Data.Sequence as Seq (Seq (Empty))
import qualified Data.Text as T
import Text.Pandoc.Builder (Blocks, Many (..), blockQuote, codeBlock, header, para)
import Text.Pandoc.Class
import Text.Pandoc.Definition

toPandoc :: (PandocMonad m) => [AutomergeSpan] -> m Pandoc
toPandoc spans = pure . Pandoc nullMeta . toList $ convertAutomergeSpans spans

convertAutomergeSpans :: [AutomergeSpan] -> Blocks
convertAutomergeSpans = foldl' convertAutomergeSpan (Many Seq.Empty)

convertAutomergeSpan :: Blocks -> AutomergeSpan -> Blocks
convertAutomergeSpan acc (BlockSpan blockSpan) = acc <> convertBlockSpan blockSpan
convertAutomergeSpan acc (TextSpan textSpan) = Many Seq.Empty

convertBlockSpan :: BlockMarker -> Blocks
convertBlockSpan blockSpan = case blockSpan of
  ParagraphMarker -> para (Many Seq.Empty)
  HeadingMarker (Heading (HeadingLevel level)) -> header level $ Many Seq.Empty
  CodeBlockMarker -> codeBlock T.empty
  BlockQuoteMarker -> blockQuote $ Many Seq.Empty
  _ -> undefined -- To be implemented
