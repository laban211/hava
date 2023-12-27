import qualified CalcSpec
import qualified Test.HUnit.Base               as HU
import qualified Test.HUnit.Text               as HU

main :: IO ()
main = do
    calcCounts <- HU.runTestTT CalcSpec.tests
    print calcCounts

    putStrLn "Test spec finished"
