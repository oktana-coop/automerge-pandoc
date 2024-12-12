module Main (main) where

import Automerge (parseAutomergeSpans)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> processJSON $ BL.pack input
    _ -> putStrLn "Usage: main \"<json_string>\""

processJSON :: BL.ByteString -> IO ()
processJSON input =
  case parseAutomergeSpans input of
    Left err -> putStrLn $ "Error: " ++ err
    Right spans -> print spans
