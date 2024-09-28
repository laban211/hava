module Types.PrintableCell
    ( PrintableCell(..)
    , createCell
    , createDefaultCell
    , createCellFromText
    , createDefaultCellFromText
    , TextAlign(..)
    ) where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

data TextAlign = LeftAlign | RightAlign deriving (Show, Eq)

data PrintableCell = PrintableCell
    { content   :: Text
    , maxLength :: Int
    , textAlign :: TextAlign
    }

createDefaultCell :: String -> Int -> PrintableCell
--                                                 default textAlign
createDefaultCell title len = createCell title len LeftAlign

createCell :: String -> Int -> TextAlign -> PrintableCell
createCell title = createCellFromText (T.pack title)

createDefaultCellFromText :: Text -> Int -> PrintableCell
createDefaultCellFromText title len =
    --                           default text align
    createCellFromText title len LeftAlign

createCellFromText :: Text -> Int -> TextAlign -> PrintableCell
createCellFromText title len textAlign =
    PrintableCell { content = title, maxLength = len, textAlign = textAlign }
