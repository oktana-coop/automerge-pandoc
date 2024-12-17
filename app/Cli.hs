module Cli (Command (..), readInputCommand) where

import Options.Applicative (Parser, argument, command, execParser, fullDesc, helper, info, metavar, progDesc, str, subparser, (<**>))

data Command
  = ConvertFromAutomerge String
  | ConvertToAutomerge String
  deriving (Show)

commandParser :: Parser Command
commandParser =
  subparser
    ( command "fromAutomerge" (info (ConvertFromAutomerge <$> argument str (metavar "AUTOMERGE_SPANS_JSON")) (progDesc "Convert from Automerge Spans JSON to Markdown"))
        <> command "toAutomerge" (info (ConvertToAutomerge <$> argument str (metavar "MARKDOWN_DATA")) (progDesc "Convert from Markdown to Automerge Spans JSON"))
    )

readInputCommand :: IO Command
readInputCommand = execParser (info (commandParser <**> helper) fullDesc)
