module Main (main) where

import Cli (Command (..), Format (..), readInputCommand)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import PandocReader (readAutomerge)
import PandocWriter (writeAutomerge)
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
  Cli.Automerge -> writeAutomerge

readFrom :: Format -> ReaderOptions -> T.Text -> PandocIO Pandoc
readFrom format = case format of
  Cli.Pandoc -> readNative
  Cli.Markdown -> readMarkdown
  Cli.Html -> readHtml
  Cli.Json -> readJSON
  Cli.Automerge -> readAutomerge

convert :: Format -> Format -> String -> IO ()
convert inputFormat outputFormat input = do
  result <- runIO $ do
    doc <- readFrom inputFormat def (T.pack input)
    writeTo outputFormat def doc
  rst <- handleError result
  TIO.putStrLn rst

convertFromAutomerge :: Format -> String -> IO ()
convertFromAutomerge outputFormat = convert Automerge outputFormat

convertToAutomerge :: Format -> String -> IO ()
convertToAutomerge inputFormat = convert inputFormat Automerge

main :: IO ()
main = do
  command <- readInputCommand
  case command of
    ConvertFromAutomerge format jsonString -> convertFromAutomerge format jsonString
    ConvertToAutomerge format markdownString -> convertToAutomerge format markdownString
