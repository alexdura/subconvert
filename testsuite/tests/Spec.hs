import Test.Hspec
import qualified SrtParserSpec
import qualified SubParserSpec
import qualified SrtPrinterSpec
main = hspec $ do {
  SrtParserSpec.tests;
  SubParserSpec.tests;
  SrtPrinterSpec.tests;
  }
