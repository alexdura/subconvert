import Test.Hspec
import qualified SrtParserSpec
import qualified SubParserSpec
main = hspec $ do {
  SrtParserSpec.tests;
  SubParserSpec.tests;
  }
