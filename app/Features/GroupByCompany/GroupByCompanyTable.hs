module Features.GroupByCompany.GroupByCompanyTable
  ( createGroupByCompanyTable,
    defaultGroupByCompanySortOption,
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
import Data.Char (toLower)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import Features.GroupByCompany.GroupByCompanyArgParse
import Features.GroupByCompany.GroupByCompanyTypes
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
import Types.Table.SortOption (SortOption (..), SortOrder (..), parseOrder)
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
  UiSize -> Int -> [String] -> [SortedByDateList ParsedTransaction] -> Text
createGroupByCompanyTable uiSize termWidth cliFlags rows =
  case parseCliFlags cliFlags of
    Left errMsg -> T.pack errMsg
    Right parsedCliFlags ->
      let header = groupByCompanyHeader
          mapContent row = createGroupByCompanyRow row header
          groupByCompanyContentRows = map mapContent rows

          -- apply cli flags

          -- apply sort
          sortedRows = sortGroupByCompanyRows (sortOptions parsedCliFlags) groupByCompanyContentRows

          -- apply limit
          limitedRows = case limit parsedCliFlags of
            Just n -> take n sortedRows
            Nothing -> sortedRows

          content = map (groupByCompanyRowToCells header) limitedRows
          spacing = map width header
       in createPrettyTable
            uiSize
            termWidth
            header
            content
            spacing

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

-- | Transforms a list of parsed transactions that shoud belong to the same company into printable
--   cells
createGroupByCompanyRow ::
  SortedByDateList ParsedTransaction -> [PrintableCell] -> GroupByCompanyContentRow
createGroupByCompanyRow rows header =
  let company =
        getCompany . extractGenericTransaction . head $
          getSortedByDateList
            rows
      buySellRows = extractBuySellRows rows
      dividendRows = extractDividendRows rows
   in GroupByCompanyContentRow
        { company = company,
          bought = calcNumBought buySellRows,
          sold = calcNumSold buySellRows,
          currentAmmount = calcRemainingAmount (getSortedByDateList rows),
          profit = calcTotalBuySellProfit buySellRows,
          dividence = calcTotalDividendProfit dividendRows,
          profitForSold = calcTotalProfitForCompany rows
        }

sortGroupByCompanyRows ::
  GroupByCompanySortOption ->
  [GroupByCompanyContentRow] ->
  [GroupByCompanyContentRow]
sortGroupByCompanyRows (GroupByCompanySortOption (SortOption column order)) rows =
  case column of
    Company -> applySort (compare `on` (T.toLower . company)) rows
    Bought -> applySort (compare `on` bought) rows
    Sold -> applySort (compare `on` sold) rows
    CurrentAmount -> applySort (compare `on` currentAmmount) rows
    Profit -> applySort (compare `on` profit) rows
    Dividend -> applySort (compare `on` dividence) rows
    ProfitForSold -> applySort (compare `on` profitForSold) rows
  where
    applySort cmp = case order of
      Ascending -> sortBy cmp
      Descending -> sortBy (flip cmp)

groupByCompanyRowToCells :: [PrintableCell] -> GroupByCompanyContentRow -> [PrintableCell]
groupByCompanyRowToCells header row =
  let tableRows =
        [ company row,
          intToText $ bought row,
          intToText $ sold row,
          intToText $ currentAmmount row,
          moneyToText $ profit row,
          moneyToText $ dividence row,
          moneyToText $ profitForSold row
        ]
   in -- Use info in header to create cells with spacing
      zipWith
        ( \title headerCell ->
            createCellFromText
              title
              (width headerCell)
              (textAlign headerCell)
        )
        tableRows
        header
