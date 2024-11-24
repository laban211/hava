module Types.PrintableCell
  ( PrintableCell (..),
    createCell,
    createFixedCell,
    createFilledCell,
    createDefaultCell,
    createCellFromText,
    createDefaultCellFromText,
    TextAlign (..),
    CellWidth (..),
    FixedWidth (..),
    isFill,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

data TextAlign = LeftAlign | RightAlign deriving (Show, Eq)

data FixedWidth = FixedWidth
  { s :: Int,
    l :: Maybe Int
  }
  deriving (Show, Eq)

data CellWidth = Fixed FixedWidth | Fill deriving (Show, Eq)

data PrintableCell = PrintableCell
  { content :: Text,
    width :: CellWidth,
    textAlign :: TextAlign
  }
  deriving (Show, Eq)

createDefaultCell :: String -> FixedWidth -> PrintableCell
--                                                 default textAlign
createDefaultCell title width = createFixedCell title width LeftAlign

createCell :: String -> CellWidth -> TextAlign -> PrintableCell
createCell title = createCellFromText (T.pack title)

createFixedCell :: String -> FixedWidth -> TextAlign -> PrintableCell
createFixedCell title fixedWidth = createCell title (Fixed fixedWidth)

createFilledCell :: String -> TextAlign -> PrintableCell
createFilledCell title textAlign =
  PrintableCell
    { content = T.pack title,
      width = Fill,
      textAlign = textAlign
    }

createDefaultCellFromText :: Text -> CellWidth -> PrintableCell
createDefaultCellFromText title width =
  --                           default text align
  createCellFromText title width LeftAlign

createCellFromText :: Text -> CellWidth -> TextAlign -> PrintableCell
createCellFromText title width textAlign =
  PrintableCell {content = title, width = width, textAlign = textAlign}

isFill :: CellWidth -> Bool
isFill Fill = True
isFill (Fixed _) = False
