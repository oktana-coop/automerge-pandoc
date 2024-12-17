module Main (main) where

import Automerge (parseAutomergeSpans)
import Cli (Command (..), Format (..), readInputCommand)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import PandocReader (toPandoc)
import PandocWriter (writeAutomergeSpans)
import Text.Pandoc (Pandoc, PandocMonad, WriterOptions, def, readMarkdown)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Writers (writeHtml5String, writeMarkdown, writeNative)

writeTo :: (PandocMonad m) => Format -> WriterOptions -> Pandoc -> m T.Text
writeTo format = case format of
  Cli.Pandoc -> writeNative
  Cli.Automerge -> writeAutomergeSpans
  Cli.Markdown -> writeMarkdown
  Cli.HTML -> writeHtml5String

convertFromAutomerge :: Format -> String -> IO ()
convertFromAutomerge format input = do
  let automergeSpans = parseAutomergeSpans $ BL.pack input
  case automergeSpans of
    Left err -> putStrLn $ "Error: " ++ err
    Right spans -> do
      result <- runIO $ do
        doc <- toPandoc spans
        writeTo format def doc
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
    ConvertFromAutomerge format jsonString -> convertFromAutomerge format jsonString
    ConvertToAutomerge markdownString -> convertToAutomerge markdownString
