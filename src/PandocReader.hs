module PandocReader where

import Automerge (AutomergeSpan)
import Data.List (foldl')
import Text.Pandoc.Class
import Text.Pandoc.Definition

convertAutomergeSpans :: (PandocMonad m) => [AutomergeSpan] -> m Pandoc
convertAutomergeSpans spans = pure . Pandoc nullMeta $ foldl' convertAutomergeSpan [] spans

convertAutomergeSpan :: [Block] -> AutomergeSpan -> [Block]
convertAutomergeSpan _ _ = []