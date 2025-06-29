module Util
  ( alwaysNegative,
    SortableByDate (..),
    unsnoc,
  )
where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text
  ( Text,
    unpack,
  )
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format
  ( defaultTimeLocale,
    parseTimeM,
  )
import Types.Transaction.GenericTransaction
  ( GenericTransaction (..),
  )
import Types.UtilTypes (SortedByDateList (..))

class SortableByDate a where
  getDate :: a -> Text

  -- Sort by date: High-Low
  sortByDate :: [a] -> SortedByDateList a
  sortByDate = SortedByDateList . sortBy compareByDate
    where
      compareByDate row1 row2 = case ( parseDate dateFormat (getDate row1),
                                       parseDate dateFormat (getDate row2)
                                     ) of
        (Just date1, Just date2) -> compare date2 date1
        _ -> EQ

-- Format: YYYY-MM-dd
dateFormat :: String
dateFormat = "%Y-%m-%d"

parseDate :: String -> Text -> Maybe UTCTime
parseDate format = parseTimeM True defaultTimeLocale format . unpack

alwaysNegative :: (Num a, Ord a) => a -> a
alwaysNegative x
  | x >= 0 = negate x
  | otherwise = x

--- | Oposite of snoc, aka (:). Given a list, it return the rest elements at the start rather than the last element
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc [x, y] = Just ([x], y)
unsnoc xs = Just (init xs, last xs)
