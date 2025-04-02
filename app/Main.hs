module Main (main) where

import Automerge (parseAutomergeSpans)
import Cli (Command (..), Format (..), readInputCommand)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import PandocReader (toPandoc)
import PandocWriter (writeAutomergeSpans)
import Text.Pandoc (Pandoc, PandocIO, PandocMonad, ReaderOptions, WriterOptions, def, readHtml, readJSON, readMarkdown, readNative)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Writers (writeHtml5String, writeJSON, writeMarkdown, writeNative)

writeTo :: (PandocMonad m) => Format -> WriterOptions -> Pandoc -> m T.Text
writeTo format = case format of
  Cli.Pandoc -> writeNative
  Cli.Markdown -> writeMarkdown
  Cli.Html -> writeHtml5String
  Cli.Json -> writeJSON

readFrom :: Format -> ReaderOptions -> T.Text -> PandocIO Pandoc
readFrom format = case format of
  Cli.Pandoc -> readNative
  Cli.Markdown -> readMarkdown
  Cli.Html -> readHtml
  Cli.Json -> readJSON

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

convertToAutomerge :: Format -> String -> IO ()
convertToAutomerge format input = do
  result <- runIO $ do
    doc <- readFrom format def (T.pack input)
    writeAutomergeSpans def doc
  rst <- handleError result
  TIO.putStrLn rst

main :: IO ()
main = do
  command <- readInputCommand
  case command of
    ConvertFromAutomerge format jsonString -> convertFromAutomerge format jsonString
    ConvertToAutomerge format markdownString -> convertToAutomerge format markdownString
