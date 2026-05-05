import PandocReaderTest as Reader (tests)
import PandocWriterTest as Writer (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  readerTests <- Reader.tests
  writerTests <- Writer.tests
  defaultMain $ testGroup "Tests" [readerTests, writerTests]
