module PrettyPrint
  ( createBuySellTable
  , createGroupByCompanyTable
  , createTableRow
  , createColumnsRow
  ) where

import           Calc                           ( calcNumBought
                                                , calcNumSold
                                                , calcRemainingAmount
                                                , calcTotalBuySellProfit
                                                , calcTotalDividendProfit
                                                , calcTotalProfitForCompany
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
import           Types.Money                    ( Money(unMoney) )
import           Types.PrintableCell            ( PrintableCell(..)
                                                , TextAlign(..)
                                                , createCell
                                                , createCellFromText
                                                , createDefaultCell
                                                , createDefaultCellFromText
                                                )
import           Types.Transaction.GenericTransaction
                                                ( GenericTransaction
                                                  ( GenericTransaction
                                                  )
                                                , getAction
                                                , getAmount
                                                , getCompany
                                                , getCourtage
                                                , getDate
                                                , getQuantity
                                                , getRate
                                                )
import           Types.Transaction.ParsedTransaction
                                                ( ParsedTransaction(..)
                                                , extractBuySellRows
                                                , extractDividendRows
                                                , extractGenericTransaction
                                                )
import           Types.Transaction.TransactionBuySell
                                                ( TransactionBuySell(..) )
import           Types.Transaction.TransactionProfitYielding
                                                ( TransactionProfitYielding )
import           Types.UtilTypes                ( SortedByDateList(..) )

createPrettyTable :: [PrintableCell] -> [[PrintableCell]] -> [Int] -> Text
createPrettyTable header content spacing =
  sep
    <> (textToLine . createTableRow $ header)
    <> sep
    <> T.unlines (map createTableRow content)
    <> sep
  where sep = textToLine $ createHorizontalSeperator spacing


-- Buy / sell

createBuySellTable :: [TransactionBuySell] -> Text
createBuySellTable rows = createPrettyTable header content spacing
 where
  header = printableBuySellHeader
  mapContent row = printableAllBuySellContent row header
  content = map mapContent rows
  spacing = map maxLength printableBuySellHeader

printableBuySellHeader :: [PrintableCell]
printableBuySellHeader =
  [ createCell "Datum"       11 LeftAlign
  , createCell "Värdepapper" 30 LeftAlign
  , createCell "Typ"         10 LeftAlign
  , createCell "Antal"       10 RightAlign
  , createCell "Kurs"        10 RightAlign
  , createCell "Belopp"      10 RightAlign
  , createCell "Courtage"    10 RightAlign
  ]


printableAllBuySellContent
  :: TransactionBuySell -> [PrintableCell] -> [PrintableCell]
printableAllBuySellContent transaction = zipWith
  (\content headerCell ->
    createCellFromText content (maxLength headerCell) (textAlign headerCell)
  )
  [ getDate transaction
  , getCompany transaction
  , T.pack (show $ getAction transaction)
  , T.pack (show $ getQuantity transaction)
  , T.pack (show $ getRate transaction)
  , T.pack (show $ getAmount transaction)
  , T.pack (show $ getCourtage transaction)
  ]

-- Group by company

createGroupByCompanyTable :: [SortedByDateList ParsedTransaction] -> Text
createGroupByCompanyTable rows = createPrettyTable header content spacing
 where
  header = groupByCompanyHeader
  mapContent row = groupByCompanyContent row header
  content = map mapContent rows
  spacing = map maxLength groupByCompanyHeader

-- | Företag | Köpt | Sålt | Nuv. balans | Vinst (kr) | Vinst sålda (kr) |
groupByCompanyHeader :: [PrintableCell]
groupByCompanyHeader = map
  (\(title, len, textAlign) -> createCell title len textAlign)
  [ ("Företag"         , 30, LeftAlign)
  , ("Köpt"            , 10, RightAlign)
  , ("Sålt"            , 10, RightAlign)
  , ("Nuv. balans"     , 10, RightAlign)
  , ("Vinst (kr)"      , 10, RightAlign)
  , ("Utdelning (kr)"  , 10, RightAlign)
  , ("Vinst sålda (kr)", 15, RightAlign)
  ]

groupByCompanyContent
  :: SortedByDateList ParsedTransaction -> [PrintableCell] -> [PrintableCell]
groupByCompanyContent rows header =
  let company =
        getCompany . extractGenericTransaction . head $ getSortedByDateList rows
      buySellRows  = extractBuySellRows rows
      dividendRows = extractDividendRows rows
      tableRows =
        [ company
        , intToText . calcNumBought $ buySellRows
        , intToText . calcNumSold $ buySellRows
        , intToText . calcRemainingAmount $ getSortedByDateList rows
        , moneyToText . calcTotalBuySellProfit $ buySellRows
        , moneyToText . calcTotalDividendProfit $ dividendRows
        , moneyToText . calcTotalProfitForCompany $ rows
        ]
  in  zipWith
        (\title headerCell -> createCellFromText title
                                                 (maxLength headerCell)
                                                 (textAlign headerCell)
        )
        tableRows
        header

intToText :: Int -> Text
intToText = T.pack . show

moneyToText :: Money -> Text
moneyToText = T.pack . printf "%.2f" . unMoney



-- General utility functions

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
cellToText x =
  let justifyFn = case textAlign x of
        LeftAlign  -> justifyLeft
        RightAlign -> justifyRight
      justifyText text len = ellipsisText len $ justifyFn len ' ' text
  in  justifyText (content x) (maxLength x)

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
