{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.AvaProfitYieldingRow
    ( AvaProfitYieldingRow(..)
    , createProfitYieldingRow
    ) where

import           Control.Applicative            ( (<|>) )
import           Data.List                      ( sortOn )
import           Data.Ord                       ( Down(Down) )
import qualified Data.Text                     as T
import           Types.AvanzaBuySellRow         ( AvanzaBuySellRow(..)
                                                , transformToBuySellRow
                                                )
import qualified Types.AvanzaBuySellRow        as AvanzaBuySellRow
import           Types.AvanzaDividendRow        ( AvanzaDividendRow(..)
                                                , transformToDividendRow
                                                )
import qualified Types.AvanzaDividendRow       as AvanzaDividendRow
import           Types.AvanzaRow                ( AvanzaRow )
import           Types.UtilTypes                ( CommonAvanzaRowFields(..)
                                                , HasAction(..)
                                                , ProfitYieldingFields(..)
                                                )

data AvaProfitYieldingRow = BuySellRowWrapper AvanzaBuySellRow
                          | DividendRowWrapper AvanzaDividendRow
                          deriving (Show)

instance HasAction AvaProfitYieldingRow where
    type ActionType AvaProfitYieldingRow
        = Either AvanzaBuySellRow.Action AvanzaDividendRow.Action
    getAction (BuySellRowWrapper  a) = Left (getAction a)
    getAction (DividendRowWrapper a) = Right (getAction a)


instance CommonAvanzaRowFields AvaProfitYieldingRow where
    getDate (BuySellRowWrapper  a) = getDate a
    getDate (DividendRowWrapper a) = getDate a

    getAccount (BuySellRowWrapper  a) = getAccount a
    getAccount (DividendRowWrapper a) = getAccount a

    getCompany (BuySellRowWrapper  a) = getCompany a
    getCompany (DividendRowWrapper a) = getCompany a

    getCurrency (BuySellRowWrapper  a) = getCurrency a
    getCurrency (DividendRowWrapper a) = getCurrency a

    getIsin (BuySellRowWrapper  a) = getIsin a
    getIsin (DividendRowWrapper a) = getIsin a

instance ProfitYieldingFields AvaProfitYieldingRow where
    getRate (BuySellRowWrapper  a) = getRate a
    getRate (DividendRowWrapper a) = getRate a

    getAmount (BuySellRowWrapper  a) = getAmount a
    getAmount (DividendRowWrapper a) = getAmount a

    getCourtage (BuySellRowWrapper  a) = getCourtage a
    getCourtage (DividendRowWrapper a) = getCourtage a

    getQuantity (BuySellRowWrapper  a) = getQuantity a
    getQuantity (DividendRowWrapper a) = getQuantity a

createProfitYieldingRow :: AvanzaRow -> Maybe AvaProfitYieldingRow
createProfitYieldingRow row =
    (transformToBuySellRow row >>= (Just . BuySellRowWrapper))
        <|> (transformToDividendRow row >>= (Just . DividendRowWrapper))

