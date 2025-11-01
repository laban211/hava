{-# LANGUAGE FlexibleInstances #-}

module Types.Transaction.TransactionBuySell
  ( TransactionBuySell (..),
    Action (..),
    transformToBuySell,
    isBuy,
    isSell,
  )
where

import qualified Data.Text as T
import Types.Money (Money)
import Types.Transaction.GenericTransaction
  ( GenericTransaction (..),
    Transaction (..),
  )
import Types.UtilTypes
  ( parseInt,
    parseMoney,
    parseMoneyDefault0,
  )
import Util (SortableByDate (..))

type TransactionBuySell = GenericTransaction Action Int Money

data Action = Buy | Sell
  deriving (Eq, Show)

transformToBuySell :: Transaction -> Maybe TransactionBuySell
transformToBuySell (GenericTransaction date account action company quantity rate amount courtage currency isin) =
  do
    action' <- case T.unpack action of
      "Köp" -> Just Buy
      "Sälj" -> Just Sell
      _ -> Nothing
    quantity' <- parseInt quantity
    rate' <- parseMoney rate
    amount' <- parseMoney amount
    -- I've noticed that courtage = 0 is sometimes indicated with "-" rather than
    -- "0", therefor the default
    let courtage' = parseMoneyDefault0 courtage

    return $
      GenericTransaction
        { action = action',
          quantity = quantity',
          rate = rate',
          amount = amount',
          courtage = courtage',
          ..
        }

instance SortableByDate TransactionBuySell where
  getDate = date

isBuy :: TransactionBuySell -> Bool
isBuy x = action x == Buy

isSell :: TransactionBuySell -> Bool
isSell x = action x == Sell
