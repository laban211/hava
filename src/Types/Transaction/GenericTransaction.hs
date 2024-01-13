module Types.Transaction.GenericTransaction
    ( GenericTransaction(..)
    , Transaction(..)
    , getAction
    , getDate
    , getCompany
    , getQuantity
    , getRate
    , getAmount
    , getCourtage
    ) where

import           Data.Text                     as T
                                                ( Text
                                                , pack
                                                )

data GenericTransaction a b c = GenericTransaction
    { date     :: !Text
    , account  :: !Text
    , action   :: !a
    , company  :: !Text
    , quantity :: !b
    , rate     :: !c
    , amount   :: !c
    , courtage :: !c
    , currency :: !Text
    , isin     :: !Text
    }
    deriving Show

type Transaction = GenericTransaction Text Text Text

getDate :: GenericTransaction a b c -> Text
getDate = date

getAction :: GenericTransaction a b c -> a
getAction = action

getCompany :: GenericTransaction a b c -> Text
getCompany = company

getQuantity :: GenericTransaction a b c -> b
getQuantity = quantity

getRate :: GenericTransaction a b c -> c
getRate = rate

getAmount :: GenericTransaction a b c -> c
getAmount = amount

getCourtage :: GenericTransaction a b c -> c
getCourtage = courtage

