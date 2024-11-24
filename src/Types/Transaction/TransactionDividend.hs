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
    parseMoney,
    parseMoneyDefault0,
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
    rate' <- parseMoney rate
    amount' <- parseMoney amount
    -- I've noticed that courtage = 0 is sometimes indicated with "-" rather than
    -- "0", therefor the default
    let courtage' = parseMoneyDefault0 courtage

    return $
      GenericTransaction
        { date = date,
          account = account,
          action = action',
          company = company,
          quantity = quantity',
          rate = rate',
          amount = amount',
          courtage = courtage',
          currency = currency,
          isin = isin
        }
