{-# LANGUAGE TypeFamilies #-}

module Types.AvanzaRow
    ( AvanzaRow(..)
    ) where

import           Data.Text                     as T
                                                ( Text
                                                , pack
                                                )
import           Types.Money                    ( Money )
import           Types.UtilTypes                ( CommonAvanzaRowFields(..)
                                                , HasAction(..)
                                                )


data AvanzaRow = AvanzaRow
    { date     :: !Text
    , account  :: !Text
    , action   :: !Text
    , company  :: !Text
    , quantity :: !Text
    , rate     :: !Text
    , amount   :: !Text
    , courtage :: !Text
    , currency :: !Text
    , isin     :: !Text
    }

instance HasAction AvanzaRow where
    type ActionType AvanzaRow = Text
    getAction = action

instance CommonAvanzaRowFields AvanzaRow where
    getDate     = date
    getAccount  = account
    getCompany  = company
    getCurrency = currency
    getIsin     = isin
