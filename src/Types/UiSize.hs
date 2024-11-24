module Types.UiSize
  ( UiSize (..),
    calcUiSize,
    uiSizeToCellWidth,
  )
where

import Data.Maybe (fromMaybe)
import Types.PrintableCell
  ( CellWidth (..),
    FixedWidth (..),
  )

data UiSize = Small | Large deriving (Show, Eq)

calcUiSize :: Maybe Int -> UiSize
calcUiSize termWidth = case termWidth of
  Just w | w >= 100 -> Large
  _ -> Small

uiSizeToCellWidth :: UiSize -> CellWidth -> Int
uiSizeToCellWidth uiSize width = case width of
  Fixed fw -> case uiSize of
    Large -> fromMaybe (s fw) (l fw)
    _ -> s fw
  Fill -> 0
