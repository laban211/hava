module Types.Table.SortOption (SortOption (..), SortOrder (..), parseOrder, allSortKeys) where

import qualified Data.Map as Map

data SortOption col = SortOption
  { column :: col,
    order :: SortOrder
  }

data SortOrder = Ascending | Descending
  deriving (Show, Eq)

sortOrderMap :: Map.Map String SortOrder
sortOrderMap =
  Map.fromList
    [ ("asc", Ascending),
      ("desc", Descending)
    ]

allSortKeys :: [String]
allSortKeys = Map.keys sortOrderMap

parseOrder :: String -> Maybe SortOrder
parseOrder key = Map.lookup key sortOrderMap
