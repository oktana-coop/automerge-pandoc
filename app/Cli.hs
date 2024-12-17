module Cli (Command (..), readInputCommand, Format (..)) where

import Options.Applicative (Parser, ReadM, argument, command, eitherReader, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, str, subparser, (<**>))

data Command
  = ConvertFromAutomerge Format String
  | ConvertToAutomerge String
  deriving (Show)

data Format = Pandoc | Automerge | Markdown | HTML deriving (Show)

formatParser :: Parser Format
formatParser = option readFormat (long "to" <> metavar "FORMAT" <> help "Specify the format (pandoc, automerge, markdown, html)")

readFormat :: ReadM Format
readFormat = eitherReader $ \arg ->
  case arg of
    "pandoc" -> Right Pandoc
    "automerge" -> Right Automerge
    "markdown" -> Right Markdown
    "html" -> Right HTML
    _ -> Left $ "Unknown format: " ++ arg

commandParser :: Parser Command
commandParser =
  subparser
    ( command "fromAutomerge" (info (ConvertFromAutomerge <$> formatParser <*> argument str (metavar "AUTOMERGE_SPANS_JSON")) (progDesc "Convert from Automerge Spans JSON to the output format"))
        <> command "toAutomerge" (info (ConvertToAutomerge <$> argument str (metavar "MARKDOWN_DATA")) (progDesc "Convert from Markdown to Automerge Spans JSON"))
    )

readInputCommand :: IO Command
readInputCommand = execParser (info (commandParser <**> helper) fullDesc)
