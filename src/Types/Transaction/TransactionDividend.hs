module Types.Transaction.TransactionDividend
  ( TransactionDividend (..),
    Action (..),
    transformToDividend,
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
    parseSvMoney,
    parseSvMoneyDefault0,
  )

type TransactionDividend = GenericTransaction Action Int Money

data Action = Dividend
  deriving (Eq, Show)

transformToDividend :: Transaction -> Maybe TransactionDividend
transformToDividend (GenericTransaction date account action company quantity rate amount courtage currency isin) =
  do
    action' <- case T.unpack action of
      "Utdelning" -> Just Dividend
      _ -> Nothing
    quantity' <- parseInt quantity
    rate' <- parseSvMoney rate
    amount' <- parseSvMoney amount
    -- I've noticed that courtage = 0 is sometimes indicated with "-" rather than
    -- "0", therefor the default
    let courtage' = parseSvMoneyDefault0 courtage

    return $
      GenericTransaction
        { action = action',
          quantity = quantity',
          rate = rate',
          amount = amount',
          courtage = courtage',
          ..
        }
