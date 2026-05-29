module Features.Positions.PositionsTable
  ( createPositionsTable,
  )
where

import Calc
  ( calcPositionCostBasis,
    calcPositionReturnPct,
    calcPositionUnrealizedProfit,
  )
import Data.Text (Text)
import qualified Data.Text as T
import PrettyPrint
  ( createPrettyTable,
    doubleToText,
    moneyToText,
  )
import Text.Printf (printf)
import Types.Money (Money (..))
import Types.Position (Position (..))
import Types.PrintableCell
  ( FixedWidth (FixedWidth, l, s),
    PrintableCell
      ( textAlign,
        width
      ),
    TextAlign
      ( LeftAlign,
        RightAlign
      ),
    createCellFromText,
    createFilledCell,
    createFixedCell,
  )
import Types.UiSize (UiSize)

createPositionsTable :: UiSize -> Int -> [Position] -> Text
createPositionsTable uiSize termWidth positions =
  let header = positionsHeader
      contentRows = map (positionRowToCells header) positions
      content = contentRows ++ [totalsRow header positions]
      spacing = map width header
   in createPrettyTable uiSize termWidth header content spacing

-- | Namn | Antal | GAV (kr) | Värde (kr) | Ansk. (kr) | Vinst (kr) | Avkastn. % |
positionsHeader :: [PrintableCell]
positionsHeader =
  [ createFilledCell "Namn" LeftAlign,
    createFixedCell "Antal" FixedWidth {s = 8, l = Just 10} RightAlign,
    createFixedCell "GAV (kr)" FixedWidth {s = 10, l = Just 12} RightAlign,
    createFixedCell "Värde (kr)" FixedWidth {s = 10, l = Just 13} RightAlign,
    createFixedCell "Ansk. (kr)" FixedWidth {s = 10, l = Just 13} RightAlign,
    createFixedCell "Vinst (kr)" FixedWidth {s = 10, l = Just 13} RightAlign,
    createFixedCell "Avkastn. %" FixedWidth {s = 9, l = Just 11} RightAlign
  ]

positionRowToCells :: [PrintableCell] -> Position -> [PrintableCell]
positionRowToCells header p =
  rowToCells header $
    [ name p,
      doubleToText (volume p),
      moneyToText (gavSek p),
      moneyToText (marketValue p),
      moneyToText (calcPositionCostBasis p),
      moneyToText (calcPositionUnrealizedProfit p),
      percentToText (calcPositionReturnPct p)
    ]

-- A summary line totalling market value, cost basis and unrealized profit across
-- every holding, plus the overall return.
totalsRow :: [PrintableCell] -> [Position] -> [PrintableCell]
totalsRow header positions =
  let totalValue = sum (map marketValue positions)
      totalCost = sum (map calcPositionCostBasis positions)
      totalProfit = totalValue - totalCost
      totalPct =
        if unMoney totalCost == 0
          then Nothing
          else Just (unMoney totalProfit / unMoney totalCost * 100)
   in rowToCells header $
        [ T.pack "Totalt",
          T.empty,
          T.empty,
          moneyToText totalValue,
          moneyToText totalCost,
          moneyToText totalProfit,
          percentToText totalPct
        ]

rowToCells :: [PrintableCell] -> [Text] -> [PrintableCell]
rowToCells header texts =
  zipWith
    (\txt h -> createCellFromText txt (width h) (textAlign h))
    texts
    header

percentToText :: Maybe Double -> Text
percentToText Nothing = T.pack "-"
percentToText (Just p) = T.pack (printf "%.2f%%" p)
