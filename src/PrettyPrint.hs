{-# OPTIONS_GHC -Wno-missing-fields #-}
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
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , justifyLeft
                                                , justifyRight
                                                )
import qualified Data.Text                     as T
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
import           Types.PrintableCell            ( CellWidth(..)
                                                , FixedWidth(..)
                                                , PrintableCell(..)
                                                , TextAlign(..)
                                                , createCell
                                                , createCellFromText
                                                , createDefaultCell
                                                , createDefaultCellFromText
                                                , createFilledCell
                                                , createFixedCell
                                                , isFill
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
import           Types.UiSize                   ( UiSize(..)
                                                , uiSizeToCellWidth
                                                )
import           Types.UtilTypes                ( SortedByDateList(..) )




createPrettyTable
  :: UiSize
  -> Int
  -> [PrintableCell]
  -> [[PrintableCell]]
  -> [CellWidth]
  -> Text
createPrettyTable uiSize termWidth header content spacing =
  let sep = textToLine $ createHorizontalSeperator $ map
        (uiSizeToCellWidth uiSize)
        spacing
      fillWidths                = calcFillWidths uiSize termWidth header
      createTableRowAppliedSize = createTableRow uiSize fillWidths
  in  sep
        <> (textToLine . createTableRowAppliedSize $ header)
        <> sep
        <> T.unlines (map createTableRowAppliedSize content)
        <> sep


-- Buy / sell

createBuySellTable :: UiSize -> Int -> [TransactionBuySell] -> Text
createBuySellTable uiSize termWidth rows = createPrettyTable uiSize
                                                             termWidth
                                                             header
                                                             content
                                                             spacing
 where
  header = printableBuySellHeader
  mapContent row = printableAllBuySellContent row header
  content = map mapContent rows
  spacing = map width printableBuySellHeader

printableBuySellHeader :: [PrintableCell]
printableBuySellHeader =
  [ createFixedCell "Datum"       FixedWidth { s = 11 } LeftAlign
  , createFixedCell "Värdepapper" FixedWidth { s = 30 } LeftAlign
  , createFixedCell "Typ"         FixedWidth { s = 10 } LeftAlign
  , createFixedCell "Antal"       FixedWidth { s = 10 } RightAlign
  , createFixedCell "Kurs"        FixedWidth { s = 10 } RightAlign
  , createFixedCell "Belopp"      FixedWidth { s = 10 } RightAlign
  , createFixedCell "Courtage"    FixedWidth { s = 10 } RightAlign
  ]


printableAllBuySellContent
  :: TransactionBuySell -> [PrintableCell] -> [PrintableCell]
printableAllBuySellContent transaction = zipWith
  (\content headerCell ->
    createCellFromText content (width headerCell) (textAlign headerCell)
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

createGroupByCompanyTable
  :: UiSize -> Int -> [SortedByDateList ParsedTransaction] -> Text
createGroupByCompanyTable uiSize termWidth rows = createPrettyTable uiSize
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
  [ createFilledCell "Företag" LeftAlign
  , createFixedCell "Köpt"        FixedWidth { s = 10, l = Just 10 } RightAlign
  , createFixedCell "Sålt"        FixedWidth { s = 10, l = Just 10 } RightAlign
  , createFixedCell "Nuv. balans" FixedWidth { s = 10, l = Just 12 } RightAlign
  , createFixedCell "Vinst (kr)"  FixedWidth { s = 10, l = Just 12 } RightAlign
  , createFixedCell "Utdelning (kr)"
                    FixedWidth { s = 10, l = Just 14 }
                    RightAlign
  , createFixedCell "Vinst sålda (kr)"
                    FixedWidth { s = 15, l = Just 17 }
                    RightAlign
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
        (\title headerCell ->
          createCellFromText title (width headerCell) (textAlign headerCell)
        )
        tableRows
        header

intToText :: Int -> Text
intToText = T.pack . show

moneyToText :: Money -> Text
moneyToText = T.pack . printf "%.2f" . unMoney



-- General utility functions

{- createTableRow :: UiSize -> Int -> [PrintableCell] -> Text
createTableRow uiSize termWidth cells =
  let sep       = T.pack "|"
      rowAsText = map (cellToText uiSize) cells
  in  sep <> T.intercalate sep rowAsText <> sep -}

createTableRow :: UiSize -> [Int] -> [PrintableCell] -> Text
createTableRow uiSize fillWidths cells =
  let sep            = T.pack "|"
      (rowAsText, _) = foldl' processCell ([], fillWidths) cells
  in  sep <> T.intercalate sep rowAsText <> sep

 where
  processCell :: ([Text], [Int]) -> PrintableCell -> ([Text], [Int])
  processCell (accText, remainingFillWidths) cell =
    let cellWidth = case width cell of
          Fill     -> head remainingFillWidths  -- Use the first available fill width
          Fixed fw -> uiSizeToCellWidth uiSize (Fixed fw)  -- Use fixed width
        remainingWidths = case width cell of
          Fill -> tail remainingFillWidths  -- Consume one fill width
          _    -> remainingFillWidths       -- Keep fill widths unchanged
        cellText = cellToText (const cellWidth) cell
    in  (accText ++ [cellText], remainingWidths)

createColumnsRow :: UiSize -> [PrintableCell] -> Int -> Text
createColumnsRow uiSize cells indentWidth =
  let indent    = T.replicate indentWidth $ T.pack " "
      rowAsText = map (cellToText $ uiSizeToCellWidth uiSize . width) cells
  in  indent <> T.intercalate indent rowAsText

-- todo: somewhere we should use fill value to fill the space..
cellToText :: (PrintableCell -> Int) -> PrintableCell -> Text
cellToText getWidth cell =
  let justifyFn = case textAlign cell of
        LeftAlign  -> justifyLeft
        RightAlign -> justifyRight
      justifyText text len = ellipsisText len $ justifyFn len ' ' text
  in  justifyText (content cell) (getWidth cell)
  -- in  justifyText (content cell) (uiSizeToCellWidth uiSize (width cell))


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

calcFillWidths :: UiSize -> Int -> [PrintableCell] -> [Int]
calcFillWidths uiSize termWidth cells =
  let fixedWidth     = sum $ map (uiSizeToCellWidth uiSize . width) cells
      fillCells      = filter (isFill . width) cells
      numOfFillCells = length fillCells
      remainingWidth = termWidth - fixedWidth
      baseWidth      = remainingWidth `div` numOfFillCells
      extraWidth     = remainingWidth `mod` numOfFillCells
  in  case fillCells of
        [] -> []
        (firstCell : restCells) ->
          (baseWidth + extraWidth) : replicate (length restCells) baseWidth
