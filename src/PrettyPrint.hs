{-# OPTIONS_GHC -Wno-missing-fields #-}
module PrettyPrint
  ( createTableRow
  , createColumnsRow
  , createPrettyTable
  , intToText
  , moneyToText
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
  let fillWidths = calcFillWidths uiSize termWidth header
      createTableRowAppliedSize = createTableRow uiSize fillWidths
      sep = textToLine $ createHorizontalSeperator $ createCellWidthsPx
        uiSize
        spacing
        fillWidths
  in  sep
        <> (textToLine . createTableRowAppliedSize $ header)
        <> sep
        <> T.unlines (map createTableRowAppliedSize content)
        <> sep

createCellWidthsPx :: UiSize -> [CellWidth] -> [Int] -> [Int]
createCellWidthsPx uiSize cellWidths fillWidths =
  let (widths, _) = foldl' processCell ([], fillWidths) cellWidths in widths
 where
  processCell :: ([Int], [Int]) -> CellWidth -> ([Int], [Int])
  processCell (accWidths, remainingFillWidths) cellWidth =
    let w = case cellWidth of
          Fill     -> head remainingFillWidths  -- Use the first available fill width
          Fixed fw -> uiSizeToCellWidth uiSize (Fixed fw)  -- Use fixed width
        remainingWidths = case cellWidth of
          Fill -> tail remainingFillWidths  -- Consume one fill width
          _    -> remainingFillWidths       -- Keep fill widths unchanged
    in  (accWidths ++ [w], remainingWidths)

intToText :: Int -> Text
intToText = T.pack . show

moneyToText :: Money -> Text
moneyToText = T.pack . printf "%.2f" . unMoney



-- General utility functions

createTableRow :: UiSize -> [Int] -> [PrintableCell] -> Text
createTableRow uiSize fillWidths cells =
  let sep            = T.pack "|"
      (textCells, _) = foldl' processCell ([], fillWidths) cells
  in  sep <> T.intercalate sep textCells <> sep

 where
  processCell :: ([Text], [Int]) -> PrintableCell -> ([Text], [Int])
  processCell (accCellTexts, remainingFillWidths) cell =
    let cellWidth = case width cell of
          Fill     -> head remainingFillWidths  -- Use the first available fill width
          Fixed fw -> uiSizeToCellWidth uiSize (Fixed fw)  -- Use fixed width
        remainingWidths = case width cell of
          Fill -> tail remainingFillWidths  -- Consume one fill width
          _    -> remainingFillWidths       -- Keep fill widths unchanged
        cellText = cellToText (const cellWidth) cell
    in  (accCellTexts ++ [cellText], remainingWidths)

createColumnsRow :: UiSize -> [PrintableCell] -> Int -> Text
createColumnsRow uiSize cells indentWidth =
  let indent    = T.replicate indentWidth $ T.pack " "
      rowAsText = map (cellToText $ uiSizeToCellWidth uiSize . width) cells
  in  indent <> T.intercalate indent rowAsText

cellToText :: (PrintableCell -> Int) -> PrintableCell -> Text
cellToText getWidth cell =
  let justifyFn = case textAlign cell of
        LeftAlign  -> justifyLeft
        RightAlign -> justifyRight
      justifyText text len = ellipsisText len $ justifyFn len ' ' text
  in  justifyText (content cell) (getWidth cell)

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
