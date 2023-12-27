{-# LANGUAGE TypeFamilies #-}

module Types.AvanzaBuySellRow
    ( AvanzaBuySellRow(..)
    , transformToBuySellRow
    , Action(..)
    , isBuy
    , isSell
    ) where

import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Text.Read                as TR
import           Text.Read                      ( readMaybe )
import qualified Types.AvanzaRow               as AvanzaRow
import           Types.AvanzaRow                ( AvanzaRow )
import           Types.Money                    ( Money(Money) )
import           Types.UtilTypes                ( CommonAvanzaRowFields(..)
                                                , HasAction(..)
                                                , ProfitYieldingFields(..)
                                                )


data AvanzaBuySellRow = AvanzaBuySellRow
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

data Action = Buy | Sell
  deriving (Eq, Show)

instance HasAction AvanzaBuySellRow where
    type ActionType AvanzaBuySellRow = Action
    getAction = action

instance CommonAvanzaRowFields AvanzaBuySellRow where
    getDate     = date
    getAccount  = account
    getCompany  = company
    getCurrency = currency
    getIsin     = isin

instance ProfitYieldingFields AvanzaBuySellRow where
    getRate     = rate
    getAmount   = amount
    getCourtage = courtage
    getQuantity = quantity

transformToBuySellRow :: AvanzaRow -> Maybe AvanzaBuySellRow
transformToBuySellRow row = do
    action' <- case T.unpack (getAction row) of
        "Köp"  -> Just Buy
        "Sälj" -> Just Sell
        _      -> Nothing
    quantity' <- parseInt $ AvanzaRow.quantity row
    rate'     <- parseMoney $ AvanzaRow.rate row
    amount'   <- parseMoney $ AvanzaRow.amount row
    -- I've noticed that courtage = 0 is sometimes indicated with "-" rather than
    -- "0", therefor the default
    let courtage' = parseMoneyDefault0 $ AvanzaRow.courtage row

    return $ AvanzaBuySellRow { date     = getDate row
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

parseInt :: Text -> Maybe Int
parseInt t = case TR.signed TR.decimal t of
    Right (n, _) -> Just n
    _            -> Nothing

parseDouble :: Text -> Maybe Double
parseDouble t = case TR.double t of
    Right (d, _) -> Just d
    _            -> Nothing

parseMoney :: Text -> Maybe Money
parseMoney t = case parseDouble t of
    Just d  -> Just $ Money d
    Nothing -> Nothing

parseMoneyDefault0 :: Text -> Money
parseMoneyDefault0 t = case parseMoney t of
    Just m  -> m
    Nothing -> Money 0.0

isBuy :: (HasAction a, ActionType a ~ Action) => a -> Bool
isBuy row = case getAction row of
    Buy -> True
    _   -> False

isSell :: (HasAction a, ActionType a ~ Action) => a -> Bool
isSell row = case getAction row of
    Sell -> True
    _    -> False
