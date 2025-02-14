import PandocReaderTest as Reader (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  readerTests <- Reader.tests
  defaultMain $ testGroup "Tests" [readerTests]
