module Cli (Command (..), readInputCommand, Format (..)) where

import Options.Applicative (Parser, ReadM, argument, command, eitherReader, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, str, subparser, (<**>))

data Command
  = ConvertFromAutomerge Format String
  | ConvertToAutomerge Format String
  deriving (Show)

data Format = Pandoc | Markdown | Html deriving (Show)

outputFormatParser :: Parser Format
outputFormatParser = option readFormat (long "to" <> metavar "FORMAT" <> help "Specify the format (pandoc, markdown, html)")

inputFormatParser :: Parser Format
inputFormatParser = option readFormat (long "from" <> metavar "FORMAT" <> help "Specify the format (pandoc, markdown, html)")

readFormat :: ReadM Format
readFormat = eitherReader $ \arg ->
  case arg of
    "pandoc" -> Right Pandoc
    "markdown" -> Right Markdown
    "html" -> Right Html
    _ -> Left $ "Unknown format: " ++ arg

commandParser :: Parser Command
commandParser =
  subparser
    ( command "fromAutomerge" (info (ConvertFromAutomerge <$> outputFormatParser <*> argument str (metavar "AUTOMERGE_SPANS_JSON")) (progDesc "Convert from Automerge Spans JSON to the output format"))
        <> command "toAutomerge" (info (ConvertToAutomerge <$> inputFormatParser <*> argument str (metavar "MARKDOWN_DATA")) (progDesc "Convert from the input format to Automerge Spans JSON"))
    )

readInputCommand :: IO Command
readInputCommand = execParser (info (commandParser <**> helper) fullDesc)
