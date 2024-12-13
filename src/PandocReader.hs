module PandocReader where

import Automerge (AutomergeSpan (..), BlockMarker (..), Heading (..), HeadingLevel (..))
import Data.Foldable (foldl', toList) -- Import foldl' from Data.Foldable
import Data.Sequence as Seq (Seq (Empty), singleton)
import Text.Pandoc.Builder (Blocks, Many (..))
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
  ParagraphMarker -> Many . singleton $ Para []
  HeadingMarker (Heading (HeadingLevel level)) -> Many . singleton $ Header level nullAttr []
  CodeBlockMarker -> undefined -- To be implemented
  _ -> undefined -- To be implemented
