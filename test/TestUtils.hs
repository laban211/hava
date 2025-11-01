module TestUtils
  ( shouldThrowErrorContaining,
  )
where

import Control.Exception (ErrorCall (..))
import Data.List (isInfixOf)
import Test.Hspec

shouldThrowErrorContaining :: IO a -> String -> Expectation
shouldThrowErrorContaining action str =
  action `shouldThrow` \(ErrorCall msg) -> str `isInfixOf` msg
