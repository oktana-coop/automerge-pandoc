module Utils.Sequence (lastValue, withoutLast) where

import Data.Sequence as Seq (length, lookup, null, take)
import Text.Pandoc.Builder (Many (Many, unMany))

-- Gets the last value of a sequence wrapped in Many
lastValue :: Many a -> Maybe a
lastValue many = Seq.lookup (Seq.length extractedSeq - 1) extractedSeq
  where
    extractedSeq = unMany many

-- Return a sequence without the last element of the input sequence
withoutLast :: Many a -> Many a
withoutLast many
  | Seq.null xs = many
  | otherwise = Many $ Seq.take (Seq.length xs - 1) xs
  where
    xs = unMany many
