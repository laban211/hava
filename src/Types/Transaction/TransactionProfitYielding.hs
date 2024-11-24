module Types.Transaction.TransactionProfitYielding
  ( TransactionProfitYielding (..),
    extractBuySellRows,
    extractDividendRows,
    extractGenericTransaction,
  )
where

import Types.Money (Money)
import Types.Transaction.GenericTransaction
  ( GenericTransaction (..),
  )
import qualified Types.Transaction.GenericTransaction as GenericTransaction
import Types.Transaction.TransactionBuySell
  ( TransactionBuySell (..),
  )
import qualified Types.Transaction.TransactionBuySell as TransactionBuySell
import Types.Transaction.TransactionDividend
  ( TransactionDividend (..),
  )
import qualified Types.Transaction.TransactionDividend as TransactionDividend
import Util (SortableByDate (..))

data TransactionProfitYielding
  = TransactionBuySellWrapper TransactionBuySell
  | TransactionDividendWrapper TransactionDividend
  deriving (Show)

data SpecificAction = SpecificBuy | SpecificSell | SpecificDividend

data ActionProfitYielding = Buy | Sell | Dividend
  deriving (Eq, Show)

-- Foldables

extractBuySellRows ::
  (Foldable f) => f TransactionProfitYielding -> [TransactionBuySell]
extractBuySellRows = foldMap extractBuySell
  where
    extractBuySell :: TransactionProfitYielding -> [TransactionBuySell]
    extractBuySell (TransactionBuySellWrapper buySellRow) = [buySellRow]
    extractBuySell _ = []

extractDividendRows ::
  (Foldable f) => f TransactionProfitYielding -> [TransactionDividend]
extractDividendRows = foldMap extractDividend
  where
    extractDividend :: TransactionProfitYielding -> [TransactionDividend]
    extractDividend (TransactionDividendWrapper dividendRow) = [dividendRow]
    extractDividend _ = []

-- TransactionProfitYielding -> GenericTransaction

extractGenericTransaction ::
  TransactionProfitYielding -> GenericTransaction SpecificAction Int Money
extractGenericTransaction (TransactionBuySellWrapper (GenericTransaction date account action company quantity rate amount courtage currency isin)) =
  GenericTransaction
    date
    account
    (convertBuySellAction action)
    company
    quantity
    rate
    amount
    courtage
    currency
    isin
extractGenericTransaction (TransactionDividendWrapper (GenericTransaction date account action company quantity rate amount courtage currency isin)) =
  GenericTransaction
    date
    account
    (convertDividendAction action)
    company
    quantity
    rate
    amount
    courtage
    currency
    isin

convertBuySellAction :: TransactionBuySell.Action -> SpecificAction
convertBuySellAction TransactionBuySell.Buy = SpecificBuy
convertBuySellAction TransactionBuySell.Sell = SpecificSell

convertDividendAction :: TransactionDividend.Action -> SpecificAction
convertDividendAction TransactionDividend.Dividend = SpecificDividend

convertActionType ::
  GenericTransaction SpecificAction Int Money ->
  GenericTransaction ActionProfitYielding Int Money
convertActionType (GenericTransaction date account action company quantity rate amount courtage currency isin) =
  GenericTransaction
    date
    account
    (convertAction action)
    company
    quantity
    rate
    amount
    courtage
    currency
    isin

convertAction :: SpecificAction -> ActionProfitYielding
convertAction specificAction = case specificAction of
  -- Mapping logic for converting specific actions to ActionProfitYielding
  SpecificBuy -> Buy
  SpecificSell -> Sell
  SpecificDividend -> Dividend

-- Type classes

instance SortableByDate TransactionProfitYielding where
  getDate x = GenericTransaction.getDate $ extractGenericTransaction x
