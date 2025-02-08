module Utils.JSON (parseStringifiedObject, stringifyObject, parseNonEmpty) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

parseStringifiedObject :: (FromJSON a) => T.Text -> Parser a
parseStringifiedObject txt = case eitherDecode $ BSL8.fromStrict $ encodeUtf8 txt of
  Left err -> fail $ "Failed to decode serialized object " ++ err
  Right val -> pure val

stringifyObject :: (ToJSON a) => a -> T.Text
stringifyObject = T.pack . BSL8.unpack . encode

parseNonEmpty :: String -> T.Text -> Parser T.Text
parseNonEmpty fieldName txt
  | T.null txt = fail $ fieldName ++ " must be non-empty"
  | otherwise = pure txt
