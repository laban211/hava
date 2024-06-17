import qualified CalcSpec
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "CalcSpec" CalcSpec.spec
