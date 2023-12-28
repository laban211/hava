module PrettyPrint
  ( createBuySellTable
  , createGroupByCompanyTable
  , createTableRow
  , createColumnsRow
  , PrintableCell(..)
  ) where

import           Calc                           ( calcNumBought
                                                , calcNumSold
                                                , calcRemainingAmount
                                                , calcTotalBuySellProfit
                                                , calcTotalDividendProfit
                                                , calcTotalProfitForCompany
                                                , extractBuySellRows
                                                , extractDividendRows
                                                , filterBuySell
                                                )
import           Data.Function                  ( (&) )
import           Data.List                      ( foldl' )
import           Data.Text                      ( Text )
import           Data.Text                     as T
                                                ( append
                                                , intercalate
                                                , justifyLeft
                                                , justifyRight
                                                , length
                                                , pack
                                                , replicate
                                                , singleton
                                                , take
                                                , unlines
                                                )
import           Debug.Trace                    ( trace )
import           GHC.Base                       ( join )
import           ParseHelper                    ( printableMoney )
import           Prelude
import           Text.Printf                    ( printf )
import           Types.AvaProfitYieldingRow     ( AvaProfitYieldingRow )
import           Types.AvanzaBuySellRow         ( AvanzaBuySellRow
                                                  ( AvanzaBuySellRow
                                                  )
                                                )
import qualified Types.AvanzaBuySellRow        as AvanzaBuySellRow
import           Types.AvanzaRow                ( AvanzaRow )
import qualified Types.AvanzaRow               as AvanzaRow
                                                ( AvanzaRow
                                                , company
                                                )
import           Types.Money                    ( Money(unMoney) )
import           Types.UtilTypes                ( CommonAvanzaRowFields
                                                  ( getCompany
                                                  )
                                                , HasAction(getAction)
                                                , SortedByDateList
                                                  ( getSortedByDateList
                                                  )
                                                )

data PrintableCell = PrintableCell
  { content   :: Text
  , maxLength :: Int
  }

createPrettyTable :: [PrintableCell] -> [[PrintableCell]] -> [Int] -> Text
createPrettyTable header content spacing =
  sep
    <> (textToLine . createTableRow $ header)
    <> sep
    <> T.unlines (map createTableRow content)
    <> sep
  where sep = textToLine $ createHorizontalSeperator spacing


-- Buy / sell

createBuySellTable :: [AvanzaBuySellRow] -> Text
createBuySellTable rows = createPrettyTable header content spacing
 where
  header  = printableBuySellHeader
  content = map printableAllBuySellContent rows
  spacing = map maxLength printableBuySellHeader

printableBuySellHeader :: [PrintableCell]
printableBuySellHeader =
  [ createCell "Datum"       11
  , createCell "Värdepapper" 30
  , createCell "Typ"         10
  , createCell "Antal"       10
  , createCell "Kurs"        10
  , createCell "Belopp"      10
  , createCell "Courtage"    10
  ]


printableAllBuySellContent :: AvanzaBuySellRow -> [PrintableCell]
printableAllBuySellContent x =
  [ PrintableCell { content = AvanzaBuySellRow.date x, maxLength = 11 }
  , PrintableCell { content = AvanzaBuySellRow.company x, maxLength = 30 }
  , PrintableCell { content = T.pack $ show $ getAction x, maxLength = 10 }
  , PrintableCell { content   = T.pack $ show (AvanzaBuySellRow.quantity x)
                  , maxLength = 10
                  }
  , PrintableCell { content   = T.pack $ show (AvanzaBuySellRow.rate x)
                  , maxLength = 10
                  }
  , PrintableCell { content   = T.pack $ show (AvanzaBuySellRow.amount x)
                  , maxLength = 10
                  }
  , PrintableCell { content   = T.pack $ show (AvanzaBuySellRow.courtage x)
                  , maxLength = 10
                  }
  ]

-- Group by company

-- todo: in the future, create an even more general type
createGroupByCompanyTable :: [SortedByDateList AvaProfitYieldingRow] -> Text
createGroupByCompanyTable rows = createPrettyTable header content spacing
 where
  header  = groupByCompanyHeader
  content = map groupByCompanyContent rows
  spacing = groupByCompanySpacing


-- | Företag | Köpt | Sålt | Nuv. balans | Vinst (kr) | Vinst sålda (kr) |
groupByCompanyHeader :: [PrintableCell]
groupByCompanyHeader = zipWith
  createCell
  [ "Företag"
  , "Köpt"
  , "Sålt"
  , "Nuv. balans"
  , "Vinst (kr)"
  , "Utdelning (kr)"
  , "Vinst sålda (kr)"
  ]
  groupByCompanySpacing

groupByCompanyContent
  :: SortedByDateList AvaProfitYieldingRow -> [PrintableCell]
groupByCompanyContent rows =
  let company      = getCompany . head $ getSortedByDateList rows
      buySellRows  = extractBuySellRows rows
      dividendRows = extractDividendRows rows
      tableRows =
        [ company
        , intToText $ calcNumBought buySellRows
        , intToText . calcNumSold $ buySellRows
        , intToText . calcRemainingAmount $ buySellRows
        , moneyToText . calcTotalBuySellProfit $ buySellRows
        , moneyToText . calcTotalDividendProfit $ dividendRows
        , moneyToText $ calcTotalProfitForCompany rows
        ]
  in  zipWith createCellFromText tableRows groupByCompanySpacing

intToText :: Int -> Text
intToText = T.pack . show

moneyToText :: Money -> Text
moneyToText = T.pack . printf "%.2f" . unMoney

groupByCompanySpacing :: [Int]
groupByCompanySpacing = [30, 10, 10, 10, 10, 10, 15]


-- General utility functions

createCell :: String -> Int -> PrintableCell
createCell title len =
  PrintableCell { content = T.pack title, maxLength = len }

createCellFromText :: Text -> Int -> PrintableCell
createCellFromText title len =
  PrintableCell { content = title, maxLength = len }

createTableRow :: [PrintableCell] -> Text
createTableRow x =
  let sep       = T.pack "|"
      rowAsText = map cellToText x
  in  sep <> T.intercalate sep rowAsText <> sep

createColumnsRow :: [PrintableCell] -> Int -> Text
createColumnsRow cells indentWidth =
  let indent    = T.replicate indentWidth $ T.pack " "
      rowAsText = map cellToText cells
  in  indent <> T.intercalate indent rowAsText

cellToText :: PrintableCell -> Text
cellToText x = justifyText (content x) (maxLength x)
  where justifyText text len = ellipsisText len $ justifyLeft len ' ' text

ellipsisText :: Int -> Text -> Text
ellipsisText len content = if T.length content > len
  then T.take (len - 1) content <> T.singleton '…'
  else content


createHorizontalSeperator :: [Int] -> Text
createHorizontalSeperator x = sep <> foldl' joinText (T.pack "") x
 where
  joinText acc elem = T.append acc (justifyText elem) <> sep
  justifyText len = justifyRight len '-' (T.pack "")
  sep = T.pack "+"

textToLine :: Text -> Text
textToLine x = x <> T.singleton '\n'

calcMaxLength :: [Text] -> Int
calcMaxLength = maximum . map T.length
