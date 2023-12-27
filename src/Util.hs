module Util
    ( sortByDate
    , alwaysNegative
    ) where

import           Data.List                      ( sortBy )
import           Data.Ord                       ( comparing )
import qualified Data.Text                     as T
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , parseTimeM
                                                )
import           Types.AvanzaBuySellRow         ( AvanzaBuySellRow )
import qualified Types.AvanzaBuySellRow        as AvanzaBuySellRow
import           Types.UtilTypes                ( CommonAvanzaRowFields(getDate)
                                                , HasAction(getAction)
                                                , SortedByDateList(..)
                                                )

-- Sort by date: High-Low
sortByDate :: CommonAvanzaRowFields a => [a] -> SortedByDateList a
sortByDate = SortedByDateList . sortBy compareByDate

-- Format: YYYY-MM-dd
dateFormat :: String
dateFormat = "%Y-%m-%d"

parseDate :: String -> Text -> Maybe UTCTime
parseDate format = parseTimeM True defaultTimeLocale format . unpack

compareByDate :: CommonAvanzaRowFields a => a -> a -> Ordering
compareByDate row1 row2 =
    case
            ( parseDate dateFormat (getDate row1)
            , parseDate dateFormat (getDate row2)
            )
        of
            (Just date1, Just date2) -> compare date2 date1
            _                        -> EQ

alwaysNegative :: (Num a, Ord a) => a -> a
alwaysNegative x | x >= 0    = negate x
                 | otherwise = x
