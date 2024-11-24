import qualified CalcSpec
import qualified ParseSpec
import qualified TablesSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "CalcSpec" CalcSpec.spec
  describe "Tables" TablesSpec.spec
  describe "Parse" ParseSpec.spec
