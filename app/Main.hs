module Main (main) where

import Automerge (parseAutomergeSpans)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.IO as TIO
import PandocReader (toPandoc)
import System.Environment (getArgs)
import Text.Pandoc (def)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Writers (writeMarkdown)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> do
      let automergeSpans = parseAutomergeSpans $ BL.pack input
      case automergeSpans of
        Left err -> putStrLn $ "Error: " ++ err
        Right spans -> do
          result <- runIO $ do
            doc <- toPandoc spans
            writeMarkdown def doc
          rst <- handleError result
          TIO.putStrLn rst
    _ -> putStrLn "Usage: main \"<json_string>\""
