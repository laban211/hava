module Tables.BuySellTable
  ( createBuySellTable,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import PrettyPrint (createPrettyTable)
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
  ( getAction,
    getAmount,
    getCompany,
    getCourtage,
    getDate,
    getQuantity,
    getRate,
  )
import Types.Transaction.ParsedTransaction
  (
  )
import Types.Transaction.TransactionBuySell
  ( TransactionBuySell,
  )
import Types.UiSize (UiSize)

createBuySellTable :: UiSize -> Int -> [TransactionBuySell] -> Text
createBuySellTable uiSize termWidth rows =
  createPrettyTable
    uiSize
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
  [ createFixedCell "Datum" FixedWidth {s = 11, l = Nothing} LeftAlign,
    createFilledCell "Värdepapper" LeftAlign,
    createFixedCell "Typ" FixedWidth {s = 10, l = Nothing} LeftAlign,
    createFixedCell "Antal" FixedWidth {s = 10, l = Nothing} RightAlign,
    createFixedCell "Kurs" FixedWidth {s = 10, l = Nothing} RightAlign,
    createFixedCell "Belopp" FixedWidth {s = 10, l = Nothing} RightAlign,
    createFixedCell "Courtage" FixedWidth {s = 10, l = Nothing} RightAlign
  ]

printableAllBuySellContent ::
  TransactionBuySell -> [PrintableCell] -> [PrintableCell]
printableAllBuySellContent transaction =
  zipWith
    ( \content headerCell ->
        createCellFromText content (width headerCell) (textAlign headerCell)
    )
    [ getDate transaction,
      getCompany transaction,
      T.pack (show $ getAction transaction),
      T.pack (show $ getQuantity transaction),
      T.pack (show $ getRate transaction),
      T.pack (show $ getAmount transaction),
      T.pack (show $ getCourtage transaction)
    ]
