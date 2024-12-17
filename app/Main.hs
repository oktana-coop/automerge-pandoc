module Main (main) where

import Automerge (parseAutomergeSpans)
import Cli (Command (..), readInputCommand)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import PandocReader (toPandoc)
import PandocWriter (writeAutomergeSpans)
import Text.Pandoc (def, readMarkdown)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Writers (writeMarkdown)

convertFromAutomerge :: String -> IO ()
convertFromAutomerge input = do
  let automergeSpans = parseAutomergeSpans $ BL.pack input
  case automergeSpans of
    Left err -> putStrLn $ "Error: " ++ err
    Right spans -> do
      result <- runIO $ do
        doc <- toPandoc spans
        writeMarkdown def doc
      rst <- handleError result
      TIO.putStrLn rst

convertToAutomerge :: String -> IO ()
convertToAutomerge input = do
  result <- runIO $ do
    doc <- readMarkdown def (T.pack input)
    writeAutomergeSpans def doc
  rst <- handleError result
  TIO.putStrLn rst

main :: IO ()
main = do
  command <- readInputCommand
  case command of
    ConvertFromAutomerge jsonString -> convertFromAutomerge jsonString
    ConvertToAutomerge markdownString -> convertToAutomerge markdownString
