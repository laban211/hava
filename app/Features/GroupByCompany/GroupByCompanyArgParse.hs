module Features.GroupByCompany.GroupByCompanyArgParse (ParsedGroupBuySellCliFlags (..), parseCliFlags, defaultGroupByCompanySortOption) where

import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Text as T
import Features.GroupByCompany.GroupByCompanyTypes
import Types.Table.SortOption (SortOption (..), SortOrder (..), allSortKeys, parseOrder)

data ParsedGroupBuySellCliFlags = ParsedGroupBuySellCliFlags
  { sortOptions :: !GroupByCompanySortOption
  -- Add more later
  }

defaultGroupByCompanySortOption :: GroupByCompanySortOption
defaultGroupByCompanySortOption = GroupByCompanySortOption (SortOption Company Descending)

parseCliFlags :: [String] -> Either String ParsedGroupBuySellCliFlags
parseCliFlags args = go args defaultGroupByCompanySortOption
  where
    go [] sortOpt = Right $ ParsedGroupBuySellCliFlags sortOpt
    go ("--sort" : col : ord : rest) _ =
      case parseGroupByCompanySortOpts (col ++ " " ++ ord) of
        Right opt -> go rest opt
        Left err -> Left $ "Failed to parse --sort flag: " ++ err
    go ("--sort" : col : rest) _ =
      case parseGroupByCompanySortOpts col of
        Right opt -> go rest opt
        Left err -> Left $ "Failed to parse --sort flag: " ++ err

parseGroupByCompanySortOpts :: String -> Either String GroupByCompanySortOption
parseGroupByCompanySortOpts input =
  case words (map toLower input) of
    [col, ord] -> do
      column <- parseCol col
      order <- parseOrd ord
      Right $ GroupByCompanySortOption (SortOption column order)
    -- Use default ordering
    [col] -> do
      column <- parseCol col
      let order' = order (unGroupByCompanySortOption defaultGroupByCompanySortOption)
      Right $ GroupByCompanySortOption (SortOption column order')
    _ -> Left "Invalid input format. Use: <column> <order> (e.g., 'profit desc')."
  where
    parseCol col =
      maybe
        ( Left $
            "Invalid column \""
              ++ col
              ++ "\". Please use one of the supported options: "
              ++ intercalate ", " allSortableColumnsKeys
        )
        Right
        $ parseSortableColumnMap col

    parseOrd ord =
      maybe
        ( Left $
            "Invalid order: \""
              ++ ord
              ++ "\". Please use one of the support options: "
              ++ intercalate ", " allSortKeys
        )
        Right
        (parseOrder ord)

sortableColumnMap :: Map.Map String SortableColumn
sortableColumnMap =
  Map.fromList
    [ (lowercaseFirst (show col), col)
      | col <- enumFrom minBound
    ]

parseSortableColumnMap :: String -> Maybe SortableColumn
parseSortableColumnMap key = Map.lookup key sortableColumnMap

allSortableColumnsKeys :: [String]
allSortableColumnsKeys = Map.keys sortableColumnMap

lowercaseFirst :: String -> String
lowercaseFirst [] = []
lowercaseFirst (x : xs) = toLower x : xs