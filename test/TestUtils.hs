module TestUtils (
  shouldThrowErrorContaining
) where 

import Control.Exception (ErrorCall(..))
import Test.Hspec 
import Data.List (isInfixOf)

shouldThrowErrorContaining :: IO a -> String -> Expectation
shouldThrowErrorContaining action str =
  action `shouldThrow` \(ErrorCall msg) -> str `isInfixOf` msg
