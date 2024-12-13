module PandocReader where

import Automerge (AutomergeSpan (..), BlockMarker (..), Heading (..), HeadingLevel (..))
import Data.Foldable (foldl', toList) -- Import foldl' from Data.Foldable
import Data.Sequence as Seq (Seq (Empty), singleton)
import Text.Pandoc.Builder (Blocks, Many (..))
import Text.Pandoc.Class
import Text.Pandoc.Definition

convertAutomergeSpans :: (PandocMonad m) => [AutomergeSpan] -> m Pandoc
convertAutomergeSpans spans = pure . Pandoc nullMeta . toList $ foldl' convertAutomergeSpan (Many Seq.Empty) spans

convertAutomergeSpan :: Blocks -> AutomergeSpan -> Blocks
convertAutomergeSpan acc (BlockSpan blockSpan) = acc <> convertBlockSpan blockSpan
convertAutomergeSpan acc (TextSpan textSpan) = Many Seq.Empty -- Empty sequence of blocks for TextSpan

convertBlockSpan :: BlockMarker -> Blocks
convertBlockSpan blockSpan = case blockSpan of
  ParagraphMarker -> Many . singleton $ Para [] -- Wrap Para [] in a singleton sequence
  HeadingMarker (Heading (HeadingLevel level)) -> Many . singleton $ Header level nullAttr []
  CodeBlockMarker -> undefined -- To be implemented
  _ -> undefined -- To be implemented
