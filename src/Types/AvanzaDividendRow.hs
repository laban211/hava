{-# LANGUAGE TypeFamilies #-}

module Types.AvanzaDividendRow
    ( AvanzaDividendRow(..)
    , Action(..)
    , transformToDividendRow
    ) where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Types.AvanzaRow               as AvanzaRow
import           Types.AvanzaRow                ( AvanzaRow(AvanzaRow) )
import           Types.Money                    ( Money(Money) )
import           Types.UtilTypes                ( CommonAvanzaRowFields(..)
                                                , HasAction(..)
                                                , ProfitYieldingFields(..)
                                                , parseInt
                                                , parseMoney
                                                , parseMoneyDefault0
                                                )

-- 2018-11-14;2296679;Utdelning;Investor A;1;4;4;-;SEK;SE0015811955;-
data AvanzaDividendRow = AvanzaDividendRow
    { date     :: !Text
    , account  :: !Text
    , action   :: !Action
    , company  :: !Text
    , quantity :: !Int
    , rate     :: !Money
    , amount   :: !Money
    , courtage :: !Money
    , currency :: !Text
    , isin     :: !Text
    }
    deriving Show

data Action = Dividend
    deriving (Eq, Show)

instance HasAction AvanzaDividendRow where
    type ActionType AvanzaDividendRow = Action
    getAction = action

instance CommonAvanzaRowFields AvanzaDividendRow where
    getDate     = date
    getAccount  = account
    getCompany  = company
    getCurrency = currency
    getIsin     = isin

instance ProfitYieldingFields AvanzaDividendRow  where
    getRate     = rate
    getAmount   = amount
    getCourtage = courtage
    getQuantity = quantity

transformToDividendRow :: AvanzaRow -> Maybe AvanzaDividendRow
transformToDividendRow row = do
    action' <- case T.unpack (getAction row) of
        "Utdelning" -> Just Dividend
        _           -> Nothing
    quantity' <- parseInt $ AvanzaRow.quantity row
    rate'     <- parseMoney $ AvanzaRow.rate row
    amount'   <- parseMoney $ AvanzaRow.amount row
    -- I've noticed that courtage = 0 is sometimes indicated with "-" rather than
    -- "0", therefor the default
    let courtage' = parseMoneyDefault0 $ AvanzaRow.courtage row

    return $ AvanzaDividendRow { date     = getDate row
                               , account  = getAccount row
                               , action   = action'
                               , company  = getCompany row
                               , quantity = quantity'
                               , rate     = rate'
                               , amount   = amount'
                               , courtage = courtage'
                               , currency = getCurrency row
                               , isin     = getIsin row
                               }

