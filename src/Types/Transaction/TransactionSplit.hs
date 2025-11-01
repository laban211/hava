module Types.Transaction.TransactionSplit
  ( TransactionSplit (..),
    Action (..),
    transformToSplit,
  )
where

import Control.Monad (guard)
import qualified Data.Text as T
import Types.Transaction.GenericTransaction
  ( GenericTransaction (..),
    Transaction,
  )
import Types.UtilTypes (parseInt)

type TransactionSplit = GenericTransaction Action Int ()

data Action = Split
  deriving (Eq, Show)

transformToSplit :: Transaction -> Maybe TransactionSplit
transformToSplit (GenericTransaction date account action company quantity rate amount courtage currency isin) =
  do
    action' <- case T.unpack action of
      "Övrigt" -> Just Split
      _ -> Nothing
    quantity' <- parseInt quantity

    -- Check that rate, amount, and courtage are "-", indicating no value
    let na = T.pack "-"
    guard (rate == na && amount == na && courtage == T.pack "0")

    return $
      GenericTransaction
        { action = action',
          quantity = quantity',
          rate = (),
          amount = (),
          courtage = (),
          ..
        }
