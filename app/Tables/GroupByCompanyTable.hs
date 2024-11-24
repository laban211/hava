module Tables.GroupByCompanyTable
  ( createGroupByCompanyTable,
  )
where

import Calc
  ( calcNumBought,
    calcNumSold,
    calcRemainingAmount,
    calcTotalBuySellProfit,
    calcTotalDividendProfit,
    calcTotalProfitForCompany,
  )
import Data.Text (Text)
import PrettyPrint
  ( createPrettyTable,
    intToText,
    moneyToText,
  )
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
import Types.Transaction.GenericTransaction
  ( getCompany,
  )
import Types.Transaction.ParsedTransaction
  ( ParsedTransaction,
    extractBuySellRows,
    extractDividendRows,
    extractGenericTransaction,
  )
import Types.UiSize (UiSize)
import Types.UtilTypes
  ( SortedByDateList
      ( getSortedByDateList
      ),
  )

createGroupByCompanyTable ::
  UiSize -> Int -> [SortedByDateList ParsedTransaction] -> Text
createGroupByCompanyTable uiSize termWidth rows =
  createPrettyTable
    uiSize
    termWidth
    header
    content
    spacing
  where
    header = groupByCompanyHeader
    mapContent row = groupByCompanyContent row header
    content = map mapContent rows
    spacing = map width header

-- | Företag | Köpt | Sålt | Nuv. balans | Vinst (kr) | Vinst sålda (kr) |
groupByCompanyHeader :: [PrintableCell]
groupByCompanyHeader =
  [ createFilledCell "Företag" LeftAlign,
    createFixedCell "Köpt" FixedWidth {s = 10, l = Just 10} RightAlign,
    createFixedCell "Sålt" FixedWidth {s = 10, l = Just 10} RightAlign,
    createFixedCell
      "Nuv. balans"
      FixedWidth {s = 10, l = Just 12}
      RightAlign,
    createFixedCell "Vinst (kr)" FixedWidth {s = 10, l = Just 12} RightAlign,
    createFixedCell
      "Utdelning (kr)"
      FixedWidth {s = 10, l = Just 14}
      RightAlign,
    createFixedCell
      "Vinst sålda (kr)"
      FixedWidth {s = 15, l = Just 17}
      RightAlign
  ]

groupByCompanyContent ::
  SortedByDateList ParsedTransaction -> [PrintableCell] -> [PrintableCell]
groupByCompanyContent rows header =
  let company =
        getCompany . extractGenericTransaction . head $
          getSortedByDateList
            rows
      buySellRows = extractBuySellRows rows
      dividendRows = extractDividendRows rows
      tableRows =
        [ company,
          intToText . calcNumBought $ buySellRows,
          intToText . calcNumSold $ buySellRows,
          intToText . calcRemainingAmount $ getSortedByDateList rows,
          moneyToText . calcTotalBuySellProfit $ buySellRows,
          moneyToText . calcTotalDividendProfit $ dividendRows,
          moneyToText . calcTotalProfitForCompany $ rows
        ]
   in zipWith
        ( \title headerCell ->
            createCellFromText
              title
              (width headerCell)
              (textAlign headerCell)
        )
        tableRows
        header
