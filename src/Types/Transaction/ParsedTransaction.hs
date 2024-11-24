{-# LANGUAGE InstanceSigs #-}

module Types.Transaction.ParsedTransaction where

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Debug.Trace
import GHC.Unicode (isDigit)
import Types.Money (Money)
import Types.Transaction.GenericTransaction
  ( GenericTransaction (..),
    Transaction,
  )
import Types.Transaction.TransactionBuySell
  ( TransactionBuySell,
    transformToBuySell,
  )
import Types.Transaction.TransactionDividend
  ( TransactionDividend,
    transformToDividend,
  )
import Types.Transaction.TransactionSplit
  ( TransactionSplit,
    transformToSplit,
  )
import Util (SortableByDate (getDate))

data ParsedTransaction
  = TransactionBuySell TransactionBuySell
  | TransactionDividend TransactionDividend
  | TransactionSplit TransactionSplit
  deriving (Show)

mapMaybeParsedTransaction :: [Transaction] -> [ParsedTransaction]
mapMaybeParsedTransaction = mapMaybe classifyTransaction

classifyTransaction :: Transaction -> Maybe ParsedTransaction
classifyTransaction tx@GenericTransaction {action = action} =
  case T.unpack action of
    "Köp" -> fmap TransactionBuySell (transformToBuySell tx)
    "Sälj" -> fmap TransactionBuySell (transformToBuySell tx)
    "Utdelning" -> fmap TransactionDividend (transformToDividend tx)
    "Övrigt" -> classifyOther tx
    _ -> Nothing

-- Quite a few misc actions exists bellow the "Övrigt"-action, make a best
-- effort in classifying the type for the transaction
classifyOther :: Transaction -> Maybe ParsedTransaction
classifyOther tx
  | isSplitTransaction tx = fmap TransactionSplit (transformToSplit tx)
  | otherwise = Nothing

isSplitTransaction :: Transaction -> Bool
isSplitTransaction GenericTransaction {quantity = quantity, rate = rate, amount = amount, courtage = courtage, currency = currency} =
  and
    [ all isDigit (T.unpack quantity),
      rate == na,
      amount == na,
      courtage == T.pack "0"
    ]
  where
    na = T.pack "-"

extractGenericTransaction ::
  ParsedTransaction -> GenericTransaction T.Text T.Text T.Text
extractGenericTransaction (TransactionBuySell (GenericTransaction date account action company quantity rate amount courtage currency isin)) =
  GenericTransaction
    date
    account
    (T.pack "BuySell")
    company
    (T.pack $ show quantity)
    (T.pack $ show rate)
    (T.pack $ show amount)
    (T.pack $ show courtage)
    currency
    isin
extractGenericTransaction (TransactionDividend (GenericTransaction date account action company quantity rate amount courtage currency isin)) =
  GenericTransaction
    date
    account
    (T.pack "Dividend")
    company
    (T.pack $ show quantity)
    (T.pack $ show rate)
    (T.pack $ show amount)
    (T.pack $ show courtage)
    currency
    isin
extractGenericTransaction (TransactionSplit (GenericTransaction date account action company quantity rate amount courtage currency isin)) =
  GenericTransaction
    date
    account
    (T.pack "Split")
    company
    (T.pack $ show quantity)
    (T.pack $ show rate)
    (T.pack $ show amount)
    (T.pack $ show courtage)
    currency
    isin

instance SortableByDate ParsedTransaction where
  getDate :: ParsedTransaction -> T.Text
  getDate (TransactionBuySell bs) = date bs
  getDate (TransactionDividend dv) = date dv
  getDate (TransactionSplit sp) = date sp

-- Foldables

extractBuySellRows :: (Foldable f) => f ParsedTransaction -> [TransactionBuySell]
extractBuySellRows = foldMap extractBuySell
  where
    extractBuySell :: ParsedTransaction -> [TransactionBuySell]
    extractBuySell (TransactionBuySell buySellRow) = [buySellRow]
    extractBuySell _ = []

extractDividendRows ::
  (Foldable f) => f ParsedTransaction -> [TransactionDividend]
extractDividendRows = foldMap extractDividend
  where
    extractDividend :: ParsedTransaction -> [TransactionDividend]
    extractDividend (TransactionDividend dividendRow) = [dividendRow]
    extractDividend _ = []
