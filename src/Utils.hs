module Utils where

import Data.Sequence as Seq (length, lookup)
import Text.Pandoc.Builder (Many (unMany))

-- Gets the last value of a sequence wrapped in Many
lastValue :: Many a -> Maybe a
lastValue many = Seq.lookup (Seq.length extractedSeq - 1) extractedSeq
  where
    extractedSeq = unMany many